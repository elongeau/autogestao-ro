{-# OPTIONS_GHC -Wno-deprecations #-}

module Autogestao.Config (
  makeConfig,
  Config (Config, configDataStore, configLog),
  Out (..),
  DataStore (..),
)
where

import System.Envy (
  FromEnv (fromEnv),
  decode,
  env,
 )
import Toml (
  TomlCodec,
  (.=),
 )
import Toml qualified

newtype DataStore = SqliteStore {connectionString :: Text}
  deriving stock (Show, Eq)

data Out = Console | File FilePath
  deriving stock (Show, Eq)

data Config = Config
  { configDataStore :: !DataStore
  , configLog :: !Out
  }
  deriving stock (Show, Eq)

newtype ConfigFile = ConfigFile FilePath

instance FromEnv ConfigFile where
  fromEnv _ = ConfigFile <$> fmap (<> "/config.toml") (env "HOME")

defaultConfig :: Config
defaultConfig = Config (SqliteStore "./autogestao.db") Console

makeConfig :: IO Config
makeConfig =
  decode >>= \case
    Nothing -> pure defaultConfig
    Just (ConfigFile file) -> go file
  where
    go :: FilePath -> IO Config
    go file =
      Toml.decodeFileEither configCodec file <&> \case
        Left _ -> defaultConfig
        Right config -> config

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> (Toml.table dataStoreCodec "datastore" <|> pure defaultConfig.configDataStore)
      .= configDataStore
    <*> (Toml.table outputCodec "logger" <|> pure defaultConfig.configLog)
      .= configLog

dataStoreCodec :: TomlCodec DataStore
dataStoreCodec = SqliteStore <$> Toml.text "connectionString" .= connectionString

outputCodec :: TomlCodec Out
outputCodec =
  Toml.dimatch matchConsole (const Console) (Toml.bool "console")
    <|> Toml.dimatch matchFile File (Toml.string "file")

matchConsole :: Out -> Maybe Bool
matchConsole = \case
  Console -> Just True
  _ -> Nothing

matchFile :: Out -> Maybe FilePath
matchFile = \case
  File file -> Just file
  _ -> Nothing
