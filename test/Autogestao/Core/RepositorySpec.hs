module Autogestao.Core.RepositorySpec where

import Autogestao.App.SqliteRepository (mkSqliteRepository)
import Autogestao.Core.InMemoryRepository (cleanInMemoryDb, mkInMemory)
import Autogestao.Core.Repository (Query (..), Repository (..))
import Autogestao.Core.Task (Task (taskDescription, taskId), TaskId (TaskId, unId))
import Autogestao.Generators
import Autogestao.Infra.DbMigrations (runMigrations)
import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as SQL
import Safe (headMay, maximumMay)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "with an in memory repository " do
    repositorySpec withInMemoryRepository
  describe "with a SQLite repository" do
    repositorySpec withSqliteRepository

type RepositoryAndCleanUp = (Repository IO, IO ())

repositorySpec :: (ActionWith RepositoryAndCleanUp -> IO ()) -> Spec
repositorySpec withRepository = aroundAll withRepository $ do
  describe "repository respect properties" do
    it "creating then updating a task return the updated one" \(Repository {..}, cleanDb) -> hedgehog do
      task <- forAll genTask
      newDesc <- forAll genText
      let updatedTask = task {taskDescription = newDesc}
      result <- liftIO do
        cleanDb
        repositorySave task
        repositorySave updatedTask
        repositoryFind FindAllTasks
      result === [updatedTask]
    it "finding all tasks return all saved tasks" \(Repository {..}, cleanDb) -> hedgehog do
      tasks <- forAll $ distinctTasks <$> genZeroOrMany genTask
      result <- liftIO do
        cleanDb
        mapM_ repositorySave tasks
        repositoryFind FindAllTasks
      result === sortBy (compare `on` taskId) tasks
    it "finding next ID return maximum ID plus one" \(Repository {..}, cleanDb) -> hedgehog do
      tasks <- forAll $ distinctTasks <$> genZeroOrMany genTask
      result <- liftIO do
        cleanDb
        mapM_ repositorySave tasks
        repositoryFind FindNextId
      let expected =
            tasks
              & fmap (unId . taskId)
              & maximumMay
              & maybe 1 (+ 1)
              & TaskId
      result === expected
    it "finding a task by its ID return the task" \(Repository {..}, cleanDb) -> hedgehog do
      tasks <- forAll $ distinctTasks <$> genZeroOrMany genTask
      let expected = headMay tasks
      result <- liftIO do
        cleanDb
        mapM_ repositorySave tasks
        repositoryFind $ FindById $ maybe (TaskId 42) taskId expected
      result === expected

withInMemoryRepository :: (RepositoryAndCleanUp -> IO ()) -> IO ()
withInMemoryRepository = bracket initDb (const pass)
  where
    initDb :: IO RepositoryAndCleanUp
    initDb = do
      db <- newIORef Map.empty
      let inMemory = mkInMemory db
      pure (inMemory, cleanInMemoryDb db)

withSqliteRepository :: (RepositoryAndCleanUp -> IO a) -> IO a
withSqliteRepository runTest = bracket initDb SQL.close \conn -> do
  repository <- mkSqliteRepository conn
  runTest (repository, cleanDb conn)
  where
    initDb = do
      conn <- SQL.open ":memory:"
      runMigrations conn
      pure conn
    cleanDb conn = liftIO $ SQL.execute_ conn "DELETE FROM tasks"
