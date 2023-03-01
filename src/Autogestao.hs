module Autogestao (
  run,
  runWithConfig,
  setup,
) where

import Autogestao.App (mkEnv)
import Autogestao.App.Env (EnvInput (EnvInput, logOut), db)
import Autogestao.App.Monad (runApp)
import Autogestao.App.Report (Listing (TaskListing), Report (EventReport, ListingReport))
import Autogestao.App.UserInput (UserInput (UserQueryAllTasks, UserTaskCommand))
import Autogestao.Config
import Autogestao.Core.Log (logInfo)
import Autogestao.Core.Repository (Query (FindAllTasks), runQuery)
import Autogestao.Core.UseCases qualified as UC
import Autogestao.Infra.DbMigrations (runMigrations)
import Autogestao.Infra.UserInputParser
import Database.SQLite.Simple qualified as SQL
import Options.Applicative
import System.Directory

run :: IO ()
run = do
  arg <- execParser opts
  config <- makeConfig
  result <- runWithConfig config arg
  putTextLn . toText $ result

runWithConfig :: Config -> UserInput -> IO Report
runWithConfig config input = do
  _ <- setup config
  conn <- SQL.open $ toString config.configDataStore.connectionString
  runMigrations conn
  env <-
    mkEnv $
      EnvInput
        { db = conn
        , logOut = config.configLog
        }
  runApp env do
    _ <- logInfo $ "Receive input " <> show input
    case input of
      UserTaskCommand cmd -> EventReport <$> UC.handle cmd
      UserQueryAllTasks -> ListingReport . TaskListing <$> runQuery FindAllTasks

setup :: Config -> IO ()
setup (Config {..}) =
  let logFolder = case configLog of
        File folder -> folder <> "/.autogestao/logs"
        _ -> ""
   in createDirectoryIfMissing True logFolder
