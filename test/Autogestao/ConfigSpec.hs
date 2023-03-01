module Autogestao.ConfigSpec where

import Autogestao.Config
import System.Environment (
  setEnv,
  unsetEnv,
 )
import System.IO.Temp (
  withSystemTempDirectory,
 )
import Test.Hspec

spec :: Spec
spec = describe "loading configuration" do
  before_ cleanEnv do
    describe "find path of configuration file" do
      it "use default config when no file is found" do
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "./autogestao.db") Console
    around withConfigFile $ describe "read configuration from file" do
      it "read configuration from '$HOME/.config'" \file -> do
        writeFileText file $ content (SqliteStore "test.db") Console
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "test.db") Console
      it "can read a file logging configuration" \file -> do
        writeFileText file $ content (SqliteStore "test.db") (File "log.txt")
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "test.db") (File "log.txt")
      it "can read a Sqlite configuration" \file -> do
        writeFileText file $ content (SqliteStore "test.db") (File "log.txt")
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "test.db") (File "log.txt")
      it "use default DB when no configuration exists in file" \file -> do
        writeFileText file $ contentForLogging (File "log.txt")
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "./autogestao.db") (File "log.txt")
      it "use default console logger when no configuration exists in file" \file -> do
        writeFileText file $ contentForDataStore $ SqliteStore "test.db"
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "test.db") Console
      it "use default configuration when file is empty" \file -> do
        writeFileText file ""
        config <- makeConfig
        config `shouldBe` Config (SqliteStore "./autogestao.db") Console

cleanEnv :: IO ()
cleanEnv = unsetEnv "HOME"

type LogOut = Out

withConfigFile :: (FilePath -> IO ()) -> IO ()
withConfigFile runTest =
  withSystemTempDirectory "test-config" \dir -> do
    setEnv "HOME" dir
    runTest $ dir <> "/config.toml"

contentForLogging :: LogOut -> Text
contentForLogging logOut =
  unlines
    [ "[logger]"
    , case logOut of
        Console -> "console = true"
        File file' -> "file = " <> show file'
    ]

contentForDataStore :: DataStore -> Text
contentForDataStore dataStore =
  unlines
    [ "[datastore]"
    , "connectionString = " <> (show . connectionString $ dataStore)
    ]

content :: DataStore -> LogOut -> Text
content datastore logOut =
  unlines
    [ contentForDataStore datastore
    , contentForLogging logOut
    ]
