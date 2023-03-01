module Autogestao.Core.UseCases.TaskUseCasesSpec where

import Autogestao.Core.InMemoryRepository (
  cleanInMemoryDb,
  saveInMemory,
 )
import Autogestao.Core.Repository (save)
import Autogestao.Core.Task (Command (Add, Update), Task (..), TaskId (TaskId))
import Autogestao.Core.UseCases (Command (TaskCmd), handle)
import Autogestao.Envs (InMemoryDb, TestApp (unApp), TestEnvApp, mkEnv)
import Autogestao.Generators (genTaskId, genText)
import Data.Map.Strict qualified as Map
import Test.Hspec (Spec, SpecWith, before, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

spec :: Spec
spec = before withEnv $ describe "Use-cases on Task domain" do
  addTaskSpec
  updateTaskSpec

addTaskSpec :: SpecWith (TestEnvApp, InMemoryDb)
addTaskSpec = describe "Adding a new task" do
  it "use provided ID for new task" \(env, db) -> hedgehog do
    taskId@(TaskId i) <- forAll genTaskId
    desc <- forAll genText
    run env do
      cleanInMemoryDb db
      saveInMemory db $ Todo taskId ""
      handle (TaskCmd . Add $ desc) >>= save
    savedTask <- Map.lookup (TaskId $ i + 1) <$> readIORef db
    savedTask === Just (Todo (TaskId $ i + 1) desc)

updateTaskSpec :: SpecWith (TestEnvApp, InMemoryDb)
updateTaskSpec = describe "Updating a task" do
  it "do nothing when task does not exist" \(env, db) -> hedgehog do
    taskId <- forAll genTaskId
    desc <- forAll genText
    run env do
      cleanInMemoryDb db
      handle (TaskCmd $ Update taskId desc) >>= save
    tasks <- readIORef db
    tasks === Map.empty
  it "update task description when it exists" \(env, db) -> hedgehog do
    taskId <- forAll genTaskId
    initialDesc <- forAll genText
    updatedDesc <- forAll genText
    run env do
      cleanInMemoryDb db
      saveInMemory db $ Todo taskId initialDesc
      handle (TaskCmd $ Update taskId updatedDesc) >>= save
    updatedTask <- Map.lookup taskId <$> readIORef db
    updatedTask === Just (Todo taskId updatedDesc)

run :: (MonadIO m) => TestEnvApp -> TestApp a -> m a
run env = liftIO . usingReaderT env . unApp

withEnv :: IO (TestEnvApp, InMemoryDb)
withEnv = do
  db <- newIORef Map.empty
  env <- mkEnv db
  pure (env, db)
