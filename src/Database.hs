{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
module Database where

import Data.Time

import Data.Text
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
WordUsage
    word Text
    user Text
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

-- FIXME: Yaknow
dbPath :: Text
dbPath = "./marvin.sqlite"

runMarvinDbMigration :: IO ()
runMarvinDbMigration = runSqlite dbPath $ runMigration migrateAll

insertWords' :: Text -> [Text] -> IO ()
insertWords' userName words = runSqlite dbPath $ do
  time <- liftIO getCurrentTime
  sequence_ $ (\w -> insert $ WordUsage w userName time) <$> words
