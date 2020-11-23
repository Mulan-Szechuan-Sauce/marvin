{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lib (bootstrapBot) where

import System.Environment (getEnv)
import System.Random
import Control.Monad (when)
import Data.Text (Text)
import Data.Either
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Read (decimal)
import Control.Applicative

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO

import Polysemy
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.IO

import Discord
import Discord.Types hiding (Embed)
import Discord.Internal.Rest.Prelude (Request)
import qualified Discord.Requests as R
import Control.Lens

import Polysemy.State

data CmdInfo = CmdInfo
  { _message :: Message
  , _name :: Text
  , _args :: [Text] }

(makeLenses ''CmdInfo)

data DiscordEff m a where
  CallDiscord :: (FromJSON a, Request (r a))
              => r a
              -> DiscordEff m (Either RestCallErrorCode a)

(makeSem ''DiscordEff)

discordEffToHandler :: Member (Embed DiscordHandler) r
                    => Sem (DiscordEff ': r) a -> Sem r a
discordEffToHandler = interpret $ \x ->
  case x of
    CallDiscord msg -> embed $ restCall msg

type Command a = forall r.
  (Members '[DiscordEff, Fail, Reader CmdInfo] r, Members a r) => Sem r ()

bootstrapBot :: IO ()
bootstrapBot = do
  token <- T.pack <$> getEnv "MARVIN_TOKEN"
  userFacingError <- runDiscord $ def { discordToken = token
                                      , discordOnEvent = eventHandler }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event =
  case event of
    MessageCreate m -> when (not (fromBot m))
      $ runM
      . embedToMonadIO @DiscordHandler
      . discordEffToHandler
      $ handleMessage m
    _ -> pure ()

handleMessage :: Members '[DiscordEff, Embed IO] r
              => Message -> Sem r ()
handleMessage m = do
  let commands =
        [ respondShuffle,
          respondDontLikeThat,
          respondGroupBy,
          respondRoll,
          respondFlip
        ]
  case nomPrefix $ messageText m of
    Just query ->
      case T.words query of
        (name:args) -> () <$ choice (runReader (CmdInfo m name args) <$> commands)
        _           -> pure ()
    _ -> pure ()
  where
    nomPrefix :: Text -> Maybe Text
    nomPrefix input = do
      let lowered = T.toLower input

      if T.isPrefixOf "!" lowered then
        pure $ T.tail input
      else if T.isPrefixOf "hey marvin " lowered then
        pure $ T.drop 11 input
      else
        Nothing

failUnmatchedArgs :: Members '[Fail] r => Sem r ()
failUnmatchedArgs = fail "Unmatched args"

gaurdName :: Members '[Fail, Reader CmdInfo] r => Text -> Sem r ()
gaurdName expected = do
  actual <- view name <$> ask
  gaurd $ expected == T.toLower actual

respondShuffle :: Command '[Embed IO]
respondShuffle = do
  gaurdName "shuffle"
  m        <- view name <$> ask
  shuffled <- view args <$> ask >>= shuffle
  sendText (T.intercalate "\n" $ ("- " <>) <$> shuffled)

respondDontLikeThat :: Command '[]
respondDontLikeThat = do
  name <- view name <$> ask
  args <- view args <$> ask
  gaurd $ ["i", "don't", "like", "that"] == (T.toLower <$> name:args)

  sendText "I'm very sorry to hear that :cry:"

respondGroupBy :: Command '[Embed IO]
respondGroupBy = finish "groupby" "<number> ..." $ do
  args <- view args <$> ask

  num <- getArg 0 args >>= intArg
  shuffled <- shuffle $ drop 1 args
  sendText $ formatTeams $ chunk num shuffled

  where
    formatTeams :: [[Text]] -> Text
    formatTeams teams = T.intercalate "\n" $
      uncurry formatTeam <$> (zip [1..] teams)

    formatTeam :: Int -> [Text] -> Text
    formatTeam teamNum members =
      "**Group " <> T.pack (show teamNum) <> "**: " <> T.intercalate ", " members

    chunk :: Int -> [a] -> [[a]]
    chunk n l = chunk' d r l
      where (d, r) = divMod (length l) n

    chunk' :: Int -> Int -> [a] -> [[a]]
    chunk' _ _ [] = []
    chunk' d r l | r == 0 =
      let (ys, zs) = splitAt d l
      in  ys : chunk' d r zs
    chunk' d r l =
      let (ys, zs) = splitAt (d + 1) l
      in  ys : chunk' d (r - 1) zs

respondRoll :: Command '[Embed IO]
respondRoll = finish "roll" "<number>" $ do
  args <- view args <$> ask

  gaurd $ length args == 1
  cardinality <- getArg 0 args >>= intArg . trimD

  react "game_die"
  randInt <- embed $ randomRIO (1, cardinality)
  sendText $ T.pack $ show randInt

  where
    trimD :: Text -> Text
    trimD t = if T.head t == 'd'
      then T.tail t
      else t

respondFlip :: Command '[Embed IO]
respondFlip = finish "flip" "" $ do
  args <- view args <$> ask

  gaurd $ length args == 0
  randInt <- embed $ randomRIO (1 :: Int, 2)

  react "\129689" -- Coin
  sendText $ if randInt == 1 then
    "Heads :upside_down:"
  else
    "Tails :peach:"

finish :: Members '[DiscordEff, Reader CmdInfo, Fail] r
       => Text -> Text -> Sem (Fail ': r) () -> Sem r ()
finish name usage action = do
  gaurdName name
  res <- runFail action
  when (isLeft res) $ sendText $ "**Usage:** `" <> name <> " " <> usage <> "`"
  pure ()

choice :: [Sem (Fail ': r) a] -> Sem r (Either String a)
choice [] = pure $ Left "o no"
choice (x:xs) = liftA2 (<|>) (runFail x) (choice xs)

react :: Members '[DiscordEff, Reader CmdInfo] r => Text -> Sem r ()
react reaction = do
  m <- view message <$> ask
  callDiscord (R.CreateReaction (messageChannel m, messageId m) reaction)
  pure ()

sendText :: Members '[DiscordEff, Reader CmdInfo] r
         => Text -> Sem r ()
sendText t = do
  m <- view message <$> ask
  callDiscord (R.CreateMessage (messageChannel m) t) *> pure ()

gaurd :: Member Fail r
      => Bool -> Sem r ()
gaurd True  = pure ()
gaurd False = fail "Gaurd failed"

getArg :: Member Fail r => Int -> [Text] -> Sem r Text
getArg index args = maybeToFail "Arg index not avaliable" $ args ^? element index

intArg :: Member Fail r => Text -> Sem r Int
intArg arg = maybeToFail "Arg is not an int" $ decimal arg ^? _Right . _1

maybeToFail :: Member Fail r
            => String -> Maybe a -> Sem r a
maybeToFail message Nothing = fail message
maybeToFail _ (Just x)      = pure x

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

shuffle :: Members '[Embed IO] r => [a] -> Sem r [a]
shuffle []  = pure []
shuffle [x] = pure [x]
shuffle lst = do
  position <- embed $ randomRIO (0, length lst - 1)
  let (before, after) = splitAt position lst
  s <- shuffle (before <> tail after)
  pure $ head after:s
