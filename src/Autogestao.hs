module Autogestao (
  run,
  runWithConfig,
  setup,
) where

import Autogestao.App (mkEnv)
import Autogestao.Core.Log (logInfo)
import Autogestao.Core.Repository (Query (FindBy, FindById), runQuery)
import Autogestao.Core.Task (
  Command (Add, Update),
  Event (NotFound),
  Task (Task),
  TaskDescription (Desc),
  TaskNotes (NoNotes, Notes),
  TaskTag (unTag),
 )
import Autogestao.Core.UseCases qualified as UC
import Autogestao.Driven.Log (LogTarget (LogFolder))
import Autogestao.Driven.Monad (App, runApp)
import Autogestao.Driven.Report (Listing (TaskListing), Report (ErrorReport, EventReport, ListingReport))
import Autogestao.Driving.Env (EnvInput (EnvInput, logOut), db)
import Autogestao.Driving.UserInput (
  EditorAction (Edit, New),
  UserInput (OpenEditor, UserQueryTasks, UserTaskCommand),
 )
import Autogestao.Infra.Config
import Autogestao.Infra.DbMigrations (runMigrations)
import Autogestao.Infra.UserInputParser
import Database.SQLite.Simple qualified as SQL
import System.Directory

import Autogestao.Core.Event (Event (TaskEvent))
import Autogestao.Core.UseCases (Command (TaskCmd))
import Autogestao.Driving.Editor (runDefaultEditor)

run :: IO ()
run = do
  arg <- runParser
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
    runUserInput input

runUserInput :: UserInput -> App Report
runUserInput = \case
  UserTaskCommand cmd -> EventReport <$> UC.decide cmd
  UserQueryTasks criteria sorting -> ListingReport . TaskListing <$> runQuery (FindBy criteria sorting)
  OpenEditor New -> do
    content <- liftIO $ runDefaultEditor ""
    let maybeCmd = readEditorContent (decodeUtf8 content)
    handleEditor maybeCmd \desc tags notes ->
      runUserInput . UserTaskCommand . TaskCmd $ Add desc tags notes
  OpenEditor (Edit taskId) -> do
    maybeTask <- runQuery $ FindById taskId
    case maybeTask of
      Nothing -> pure . EventReport . TaskEvent . NotFound $ taskId
      Just (Task _ (Desc txtDesc) _ tags notes) -> do
        let header = txtDesc <> " " <> unwords (tags <&> \tag -> "#" <> unTag tag)
        let txtNotes = case notes of
              NoNotes -> []
              Notes txt -> ["", txt]
        content <- liftIO . runDefaultEditor . unlines $ header : txtNotes
        let maybeCmd = readEditorContent (decodeUtf8 content)
        handleEditor maybeCmd \desc tags' notes' ->
          runUserInput . UserTaskCommand . TaskCmd $ Update taskId desc tags' notes'

handleEditor :: FromEditor -> (TaskDescription -> [TaskTag] -> TaskNotes -> App Report) -> App Report
handleEditor maybeContent f = case maybeContent of
  (FromEditor desc tags notes) -> f desc tags notes
  NoContent -> pure . ErrorReport $ "Editor's empty. Nothing to do"
  ParseError errors -> pure . ErrorReport $ errors

setup :: Config -> IO ()
setup (Config {..}) =
  let (LogFolder logFolder) = configLog
   in createDirectoryIfMissing True logFolder
