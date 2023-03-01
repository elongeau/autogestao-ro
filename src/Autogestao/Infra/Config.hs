{-# OPTIONS_GHC -Wno-deprecations #-}

module Autogestao.Infra.Config (
  makeConfig,
  Config (Config, configDataStore, configLog),
  DataStore (..),
)
where

import Autogestao.Driven.Log (LogTarget (LogFolder))
import System.Directory (createDirectoryIfMissing)

newtype DataStore = SqliteStore {connectionString :: Text}
  deriving stock (Show, Eq)

data Config = Config
  { configDataStore :: !DataStore
  , configLog :: !LogTarget
  }
  deriving stock (Show, Eq)

defaultConfig :: Config
defaultConfig = Config (SqliteStore "./autogestao.db") (LogFolder "/tmp")

makeConfig :: IO Config
makeConfig =
  lookupEnv "HOME" >>= \case
    Nothing -> pure defaultConfig
    Just homeDir -> do
      let baseDir = toString $ homeDir <> "/.config/autogestao"
      createDirectoryIfMissing True baseDir
      let db = SqliteStore . toText $ baseDir <> "/tasks.db"
          log = LogFolder $ baseDir <> "/logs"
      pure $ Config db log
