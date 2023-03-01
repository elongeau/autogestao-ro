module Autogestao.Infra.ConfigSpec where

import Autogestao.Driven.Log (LogTarget (LogFolder))
import Autogestao.Infra.Config
import System.Environment (
  setEnv,
  unsetEnv,
 )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "loading configuration" do
  it "use default config when $HOME is not set" do
    unsetEnv "HOME"
    config <- makeConfig
    config `shouldBe` Config (SqliteStore "./autogestao.db") (LogFolder "/tmp")
  around setHomeEnv $ it "use default config when no file is found" \home -> do
    config <- makeConfig
    config `shouldBe` Config (SqliteStore . toText $ home <> "/.config/autogestao/tasks.db") (LogFolder $ home <> "/.config/autogestao/logs")

setHomeEnv :: (FilePath -> IO ()) -> IO ()
setHomeEnv runTest = withSystemTempDirectory "fakeDir" \fakeDir -> do
  setEnv "HOME" fakeDir
  runTest fakeDir
  unsetEnv "HOME"
