module AutogestaoSpec where

import Autogestao (
  runWithConfig,
 )
import Autogestao.Core.Event qualified as E
import Autogestao.Core.Repository (Sort (SortById), allTasks, defaultCriteria)
import Autogestao.Core.Task (
  Task (Task, taskStatus),
  TaskId (TaskId),
  TaskNotes (NoNotes),
  TaskStatus (Todo),
 )
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd))
import Autogestao.Driven.Log (LogTarget (LogFolder))
import Autogestao.Driven.Report (
  Listing (TaskListing),
  Report (EventReport, ListingReport),
 )
import Autogestao.Driving.UserInput (UserInput (UserQueryTasks, UserTaskCommand))
import Autogestao.Infra.Config (
  Config (Config),
  DataStore (SqliteStore),
 )
import System.IO.Temp (
  withSystemTempDirectory,
 )
import Test.Hspec

spec :: Spec
spec = do
  describe "Integrated tests" do
    it "should display a task that was just added" do
      result <-
        runApp
          [ UserTaskCommand . TaskCmd $ Task.Add "do something" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "assert it works" [] NoNotes
          , UserQueryTasks defaultCriteria SortById
          ]
      let task1 = Task (Task.TaskId 1) "do something" Todo [] NoNotes
      let task2 = Task (Task.TaskId 2) "assert it works" Todo [] NoNotes
      result
        `shouldBe` [ EventReport $ E.TaskEvent $ Task.Created task1
                   , EventReport $ E.TaskEvent $ Task.Created task2
                   , ListingReport $ TaskListing [task1, task2]
                   ]
    it "should update an existing task" do
      result <-
        runApp
          [ UserTaskCommand . TaskCmd $ Task.Add "do something" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Update (TaskId 1) "do something else" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 1) Task.Next
          , UserQueryTasks defaultCriteria SortById
          ]
      let taskAtCreation = Task (Task.TaskId 1) "do something" Todo [] NoNotes
      let taskAfterUpdate = Task (Task.TaskId 1) "do something else" Task.Next [] NoNotes
      result
        `shouldBe` [ EventReport . E.TaskEvent $ Task.Created taskAtCreation
                   , EventReport . E.TaskEvent $ Task.Updated taskAfterUpdate {taskStatus = Todo}
                   , EventReport . E.TaskEvent $ Task.Updated taskAfterUpdate
                   , ListingReport $ TaskListing [taskAfterUpdate]
                   ]
    it "list active tasks by default" do
      result <-
        runApp
          [ UserTaskCommand . TaskCmd $ Task.Add "todo task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "next task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "ongoing task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "done task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "canceled task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 2) Task.Next
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 3) Task.Ongoing
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 4) Task.Done
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 5) Task.Canceled
          , UserQueryTasks defaultCriteria SortById
          ]
      let todo = Task (Task.TaskId 1) "todo task" Task.Todo [] NoNotes
      let next = Task (Task.TaskId 2) "next task" Task.Next [] NoNotes
      let ongoing = Task (Task.TaskId 3) "ongoing task" Task.Ongoing [] NoNotes
      result `shouldContain` [ListingReport $ TaskListing [todo, next, ongoing]]
    it "list all tasks by default" do
      result <-
        runApp
          [ UserTaskCommand . TaskCmd $ Task.Add "todo task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "next task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "ongoing task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "done task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.Add "canceled task" [] NoNotes
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 2) Task.Next
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 3) Task.Ongoing
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 4) Task.Done
          , UserTaskCommand . TaskCmd $ Task.UpdateStatus (TaskId 5) Task.Canceled
          , UserQueryTasks allTasks SortById
          ]
      let todo = Task (Task.TaskId 1) "todo task" Task.Todo [] NoNotes
      let next = Task (Task.TaskId 2) "next task" Task.Next [] NoNotes
      let ongoing = Task (Task.TaskId 3) "ongoing task" Task.Ongoing [] NoNotes
      let done = Task (Task.TaskId 4) "done task" Task.Done [] NoNotes
      let canceled = Task (Task.TaskId 5) "canceled task" Task.Canceled [] NoNotes
      result `shouldContain` [ListingReport $ TaskListing [todo, next, ongoing, done, canceled]]

runApp :: [UserInput] -> IO [Report]
runApp inputs =
  withSystemTempDirectory "tmpDir" \baseDir -> do
    let config = Config (SqliteStore . toText $ baseDir <> "/test.db") (LogFolder baseDir)
    mapM (runWithConfig config) inputs
