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
import Polysemy.IO
import Discord 
import Discord.Types hiding (Embed)
import Discord.Internal.Rest.Prelude (Request)
import qualified Discord.Requests as R
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Polysemy.State

data DiscordEff m a where
  CallDiscord :: (FromJSON a, Request (r a))
              => r a
              -> DiscordEff m (Either RestCallErrorCode a)

(makeSem ''DiscordEff)

discordEffToHandler :: Member (Embed DiscordHandler) r => Sem (DiscordEff ': r) a -> Sem r a
discordEffToHandler = interpret $ \x ->
  case x of
    CallDiscord msg -> embed $ restCall msg

type MaybeD a = MaybeT DiscordHandler a

data Command = Command
  { _input :: NonEmpty Text
  , _message :: Message
  } deriving (Show)

(makeLenses ''Command)

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
  case nomPrefix $ messageText m of
    Just query ->
      case NE.nonEmpty $ T.words query of
        Just args -> respond args m
        _         -> pure ()
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

respond :: Members '[Embed IO, DiscordEff] r
        => NonEmpty Text -> Message -> Sem r ()
respond ("I" :| ["don't", "like", "that"]) m = do
  sendText m "I'm very sorry to hear that :cry:"
respond ("shuffle" :| args) m = do
  shuffled <- shuffle args
  sendText m (T.intercalate "\n" $ ("- " <>) <$> shuffled)
respond ("groupby" :| args) m = finish m "groupby <number> ..." $ do
  num <- getArg 0 args >>= intArg
  shuffled <- shuffle $ drop 1 args
  sendText m $ formatTeams $ chunk num shuffled

  where
    formatTeams :: [[Text]] -> Text
    formatTeams teams = T.intercalate "\n" $
      uncurry formatTeam <$> (zip [1..] teams)

    formatTeam :: Int -> [Text] -> Text
    formatTeam teamNum members =
      "**Group " <> T.pack (show teamNum) <> "**: " <> T.intercalate ", " members

    chunk :: Int -> [a] -> [[a]]
    chunk numChunks l =
      let len = toRational (length l)
          nc  = toRational numChunks
      in chunk' (ceiling $ len / nc) l

    chunk' :: Int -> [a] -> [[a]]
    chunk' _ [] = []
    chunk' n xs =
      let (ys, zs) = splitAt n xs
      in  ys : chunk' n zs

respond ("roll" :| args) m = finish m "roll <number>" $ do
  gaurd $ length args == 1
  cardinality <- getArg 0 args >>= intArg . trimD

  react m "game_die"
  randInt <- liftIO $ randomRIO (1, cardinality)
  sendText m $ T.pack $ show randInt

  where
    trimD :: Text -> Text
    trimD t = if T.head t == 'd'
      then T.tail t
      else t

finish :: Member DiscordEff r
       => Message -> Text -> Sem (Fail ': r) () -> Sem r ()
finish m message action = do
  res <- runFail action
  when (isLeft res) $ sendText m $ "**Usage:** `" <> message <> "`" 
  pure ()

choice :: Member Fail r
       => [Sem (Fail ': r) a] -> Sem r (Either String a)
choice [] = pure $ Left "o no"
choice (x:xs) = liftA2 (<|>) (runFail x) (choice xs)

react :: Member DiscordEff r => Message -> Text -> Sem r ()
react m reaction = do
  callDiscord (R.CreateReaction (messageChannel m, messageId m) reaction)
  pure ()

sendText :: Member DiscordEff r
            => Message -> Text -> Sem r ()
sendText m t = callDiscord (R.CreateMessage (messageChannel m) t) *> pure ()

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
