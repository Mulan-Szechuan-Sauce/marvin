{-# LANGUAGE OverloadedStrings #-}
module Lib (bootstrapBot) where

import System.Environment (getEnv)
import System.Random
import Control.Monad (when)
import Data.Text (Text)
import Data.Either
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Read (decimal)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

type MaybeD a = MaybeT DiscordHandler a

bootstrapBot :: IO ()
bootstrapBot = do
  token <- T.pack <$> getEnv "MARVIN_TOKEN"
  userFacingError <- runDiscord $ def { discordToken = token
                                      , discordOnEvent = eventHandler }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m)) $ handleMessage m
  _ -> pure ()

handleMessage :: Message -> DiscordHandler ()
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

respond :: NonEmpty Text -> Message -> DiscordHandler ()
respond ("I" :| ["don't", "like", "that"]) m = do
  sendText m "I'm very sorry to hear that :cry:"
respond ("shuffle" :| args) m = do
  shuffled <- liftIO $ shuffle args
  sendText m (T.intercalate "\n" $ ("- " <>) <$> shuffled)
respond ("groupby" :| args) m = finish m "groupby <number> ..." $ do
  num <- getArg 0 args >>= intArg
  shuffled <- liftIO $ shuffle $ drop 1 args
  sendTextD m $ formatTeams $ chunk num shuffled

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

  reactD m "game_die"
  randInt <- liftIO $ randomRIO (1, cardinality)
  sendTextD m $ T.pack $ show randInt

  where
    trimD :: Text -> Text
    trimD t = if T.head t == 'd'
      then T.tail t
      else t

finish :: Message -> Text -> MaybeD () -> DiscordHandler ()
finish m message action = do
  mayb <- runMaybeT action
  when (isNothing mayb) $ sendText m $ "**Usage:** `" <> message <> "`" 
  pure ()

reactD :: Message -> Text -> MaybeD ()
reactD m reaction = do
  lift $ restCall (R.CreateReaction (messageChannel m, messageId m) reaction)
  pure ()

sendTextD :: Message -> Text -> MaybeD ()
sendTextD m t = lift $ sendText m t

sendText :: Message -> Text -> DiscordHandler ()
sendText m t = restCall (R.CreateMessage (messageChannel m) t) *> pure ()

gaurd :: Bool -> MaybeD ()
gaurd True  = pure ()
gaurd False = hoistMaybe Nothing

getArg :: Int -> [Text] -> MaybeD Text
getArg index args = hoistMaybe $ args ^? element index

intArg :: Text -> MaybeD Int
intArg arg = hoistMaybe $ decimal arg ^? _Right . _1

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

shuffle :: [a] -> IO [a]
shuffle []  = pure []
shuffle [x] = pure [x]
shuffle lst = do
  position <- randomRIO (0, length lst - 1)
  let (before, after) = splitAt position lst
  s <- shuffle (before <> tail after)
  pure $ head after:s
