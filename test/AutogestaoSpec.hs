module AutogestaoSpec where

import Autogestao (
  runWithConfig,
 )
import Autogestao.App.Report (
  Listing (TaskListing),
  Report (EventReport, ListingReport),
 )
import Autogestao.App.UserInput (UserInput (UserQueryAllTasks, UserTaskCommand))
import Autogestao.Config (
  Config (Config),
  DataStore (SqliteStore),
  Out (Console),
 )
import Autogestao.Core.Event qualified as E
import Autogestao.Core.Task (
  TaskId (TaskId),
 )
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd))
import GHC.IO.Handle (hClose)
import System.IO.Temp (
  withSystemTempFile,
 )
import Test.Hspec

spec :: Spec
spec = do
  describe "Integrated tests" do
    it "should display a task that was just added" do
      result <-
        runApp
          [ UserTaskCommand . TaskCmd $ Task.Add "do something"
          , UserTaskCommand . TaskCmd $ Task.Add "assert it works"
          , UserQueryAllTasks
          ]
      let task1 = Task.Todo (Task.TaskId 1) "do something"
      let task2 = Task.Todo (Task.TaskId 2) "assert it works"
      result
        `shouldBe` [ EventReport $ E.TaskEvent $ Task.Created task1
                   , EventReport $ E.TaskEvent $ Task.Created task2
                   , ListingReport $ TaskListing [task1, task2]
                   ]
    it "should update description of an existing task" do
      result <-
        runApp
          [ UserTaskCommand . TaskCmd $ Task.Add "do something"
          , UserTaskCommand . TaskCmd $ Task.Update (TaskId 1) "do something else"
          , UserQueryAllTasks
          ]
      let taskAtCreation = Task.Todo (Task.TaskId 1) "do something"
      let taskAfterUpdate = Task.Todo (Task.TaskId 1) "do something else"
      result
        `shouldBe` [ EventReport $ E.TaskEvent $ Task.Created taskAtCreation
                   , EventReport $ E.TaskEvent $ Task.Updated taskAfterUpdate
                   , ListingReport $ TaskListing [taskAfterUpdate]
                   ]

runApp :: [UserInput] -> IO [Report]
runApp inputs =
  withSystemTempFile "test.db" \dbFile out -> do
    hClose out
    let config = Config (SqliteStore $ toText dbFile) Console
    mapM (runWithConfig config) inputs
