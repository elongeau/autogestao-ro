module Autogestao.Driven.RepositorySpec where

import Autogestao.Core.InMemoryRepository (mkInMemory)
import Autogestao.Core.Repository (
  Criteria (StatusIn, Tags),
  Query (..),
  Repository (..),
  Sort (NoSort, SortById, SortByStatus),
  allTasks,
 )
import Autogestao.Core.Task (
  Task (Task, taskDescription, taskId, taskNotes, taskStatus, taskTags),
  TaskDescription (..),
  TaskId (TaskId),
  TaskNotes (NoNotes, Notes),
  TaskStatus (..),
  TaskTag (TaskTag),
 )
import Autogestao.Driven.SqliteRepository (mkSqliteRepository)
import Autogestao.Infra.DbMigrations (runMigrations)
import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as SQL
import Test.Hspec

spec :: Spec
spec = do
  describe "with an in memory repository " do
    repositorySpec withInMemoryRepository
  describe "with a SQLite repository" do
    repositorySpec withSqliteRepository

repositorySpec :: (ActionWith (Repository IO) -> IO ()) -> Spec
repositorySpec withRepository = around withRepository $ do
  describe "repository respect properties" do
    let task1 = Task (TaskId 1) "do something" Todo ["bar", "foo"] NoNotes
    let task2 = Task (TaskId 2) "do something again" Next ["hello", "world"] NoNotes
    let task3 = Task (TaskId 3) "do something again" Ongoing [] NoNotes
    let tasks = [task1, task2, task3]
    it "creating then updating a task return the updated one" \Repository {..} -> do
      let updated =
            task1
              { taskDescription = "do something else"
              , taskStatus = Done
              , taskTags = ["baz", "qux"]
              , taskNotes = Notes "some notes"
              }
      repositorySave task1
      repositorySave updated
      result <- repositoryFind $ FindBy allTasks NoSort
      result `shouldBe` [updated]
    it "finding all tasks return all saved tasks sorted by id" \Repository {..} -> do
      mapM_ repositorySave tasks
      result <- repositoryFind $ FindBy allTasks SortById
      result `shouldBe` tasks
    it "finding all tasks return all saved tasks sorted by status" \Repository {..} -> do
      mapM_ repositorySave tasks
      result <- repositoryFind $ FindBy allTasks SortByStatus
      result `shouldBe` [task3, task2, task1]
    it "finding tasks with a specific statuts return matching tasks" \Repository {..} -> do
      mapM_ repositorySave tasks
      result <- repositoryFind $ FindBy (StatusIn [Next]) SortById
      result `shouldBe` [task2]
    it "finding next ID return maximum ID plus one" \Repository {..} -> do
      mapM_ repositorySave tasks
      result <- repositoryFind FindNextId
      result `shouldBe` TaskId 4
    it "finding a task by its ID return the task" \Repository {..} -> do
      mapM_ repositorySave tasks
      result <- repositoryFind $ FindById (TaskId 2)
      result `shouldBe` Just task2
    it "finding tasks with some tags return matching tasks" \Repository {..} -> do
      mapM_ repositorySave tasks
      result <- repositoryFind $ FindBy (Tags [TaskTag "bar", TaskTag "world"]) SortById
      result `shouldBe` [task1, task2]
    it "finds tasks with status and tags" \Repository {..} -> do
      let task4 = Task (TaskId 4) (Desc "one") Ongoing [TaskTag "foo"] NoNotes
      let otherTasks = tasks ++ [task4, Task (TaskId 5) (Desc "five") Done [TaskTag "foo"] NoNotes]
      let criteria = StatusIn [Ongoing, Next] <> Tags [TaskTag "foo"]
      mapM_ repositorySave otherTasks
      result <- repositoryFind $ FindBy criteria SortById
      result `shouldBe` [task4]

withInMemoryRepository :: (Repository IO -> IO ()) -> IO ()
withInMemoryRepository = bracket initDb (const pass)
  where
    initDb = mkInMemory <$> newIORef Map.empty

withSqliteRepository :: (Repository IO -> IO a) -> IO a
withSqliteRepository runTest = bracket initDb SQL.close \conn -> do
  repository <- mkSqliteRepository conn
  runTest repository
  where
    initDb = do
      conn <- SQL.open ""
      runMigrations conn
      pure conn

tasksFor :: TaskStatus -> [Task] -> [Task]
tasksFor status = sortBy (compare `on` taskId) . filter (\task -> taskStatus task == status)
