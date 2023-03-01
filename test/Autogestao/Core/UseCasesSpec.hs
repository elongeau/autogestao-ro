module Autogestao.Core.UseCasesSpec where

import Autogestao.Core.Event (Event (TaskEvent))
import Autogestao.Core.InMemoryRepository (
  cleanInMemoryDb,
  saveInMemory,
 )
import Autogestao.Core.Log (HasLog)
import Autogestao.Core.Repository (HasRepository)
import Autogestao.Core.Task (Command (Add, Update, UpdateStatus), Task (Task, taskId, taskStatus), TaskDescription (Desc), TaskId (TaskId), TaskNotes (NoNotes, Notes), TaskStatus (Todo), TaskTag (TaskTag, unTag))
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd), decide)
import Autogestao.Envs (InMemoryDb, TestApp (unApp), TestEnvApp, mkEnv)
import Autogestao.Generators (genNotes, genTags, genTask, genTaskDesc, genTaskId, genTaskStatus)
import Data.Map.Strict qualified as Map
import Data.Text (strip, toLower, toUpper)
import Test.Hspec (Spec, SpecWith, before, describe, it, shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

spec :: Spec
spec = before withEnv $ describe "Use-cases on Task domain" do
  addTaskSpec
  updateTaskSpec
  updateTaskStatusSpec

taskHandle :: forall env m. (HasRepository env m, HasLog env m) => Task.Command -> m Event
taskHandle = decide . TaskCmd

addTaskSpec :: SpecWith (TestEnvApp, InMemoryDb)
addTaskSpec = describe "Adding a new task" do
  it "use provided ID for new task" \(env, db) -> hedgehog do
    taskId@(TaskId i) <- forAll genTaskId
    desc <- forAll genTaskDesc
    tags <- forAll genTags
    notes <- forAll genNotes
    event <- run env do
      cleanInMemoryDb db
      saveInMemory db $ Task taskId "" Todo [] NoNotes
      taskHandle $ Add desc tags notes
    let expected = Task (TaskId $ i + 1) desc Todo tags notes
    event === TaskEvent (Task.Created expected)
    savedTask <- Map.lookup (TaskId $ i + 1) <$> readIORef db
    savedTask === Just expected
  it "lowercase tags" \(env, db) -> hedgehog do
    desc <- forAll genTaskDesc
    tags <- forAll $ fmap (TaskTag . toUpper . unTag) <$> genTags
    event <- run env do
      cleanInMemoryDb db
      taskHandle $ Add desc tags NoNotes
    event === (TaskEvent . Task.Created $ Task (TaskId 1) desc Todo (fmap (TaskTag . toLower . unTag) tags) NoNotes)
  it "remove leading and trailing lines on notes" \(env, db) -> do
    event <- run env do
      cleanInMemoryDb db
      taskHandle $ Add (Desc "desc") [] (Notes . unlines $ ["", "", "foo", "", "bar", "", ""])
    event `shouldBe` (TaskEvent . Task.Created $ Task (TaskId 1) (Desc "desc") Todo [] (Notes . strip . unlines $ ["foo", "", "bar"]))

updateTaskSpec :: SpecWith (TestEnvApp, InMemoryDb)
updateTaskSpec = describe "Updating a task" do
  it "do nothing when task does not exist" \(env, db) -> hedgehog do
    taskId <- forAll genTaskId
    desc <- forAll genTaskDesc
    event <- run env do
      cleanInMemoryDb db
      taskHandle $ Update taskId desc [] NoNotes
    event === TaskEvent (Task.NotFound taskId)
    tasks <- readIORef db
    tasks === Map.empty
  it "update description, notes and tags when it exists" \(env, db) -> hedgehog do
    taskId <- forAll genTaskId
    initialDesc <- forAll genTaskDesc
    initialTags <- forAll genTags
    initialNotes <- forAll genNotes
    updatedDesc <- forAll genTaskDesc
    updatedTags <- forAll genTags
    updatedNotes <- forAll genNotes
    event <- run env do
      cleanInMemoryDb db
      saveInMemory db $ Task taskId initialDesc Todo initialTags initialNotes
      taskHandle $ Update taskId updatedDesc updatedTags updatedNotes
    let expected = Task taskId updatedDesc Todo updatedTags updatedNotes
    event === TaskEvent (Task.Updated expected)
    updatedTask <- Map.lookup taskId <$> readIORef db
    updatedTask === Just expected
  it "lowercase tags" \(env, db) -> hedgehog do
    taskId <- forAll genTaskId
    desc <- forAll genTaskDesc
    tags <- forAll $ fmap (TaskTag . toUpper . unTag) <$> genTags
    event <- run env do
      cleanInMemoryDb db
      saveInMemory db $ Task taskId desc Todo tags NoNotes
      taskHandle $ Update taskId desc tags NoNotes
    event === (TaskEvent . Task.Updated $ Task taskId desc Todo (fmap (TaskTag . toLower . unTag) tags) NoNotes)
  it "remove leading and trailing lines on notes" \(env, db) -> do
    event <- run env do
      cleanInMemoryDb db
      saveInMemory db $ Task (TaskId 1) (Desc "desc") Todo [] NoNotes
      taskHandle $ Update (TaskId 1) (Desc "desc") [] (Notes . unlines $ ["", "", "foo", "", "bar", "", ""])
    event `shouldBe` (TaskEvent . Task.Updated $ Task (TaskId 1) (Desc "desc") Todo [] (Notes . strip . unlines $ ["foo", "", "bar"]))

updateTaskStatusSpec :: SpecWith (TestEnvApp, InMemoryDb)
updateTaskStatusSpec = describe "updating status" do
  it "do nothing when task is unknown" \(env, db) -> hedgehog do
    (taskId, newStatus) <- forAll $ (,) <$> genTaskId <*> genTaskStatus
    event <- run env do
      taskHandle $ UpdateStatus taskId newStatus
    event === TaskEvent (Task.NotFound taskId)
    tasks <- readIORef db
    tasks === Map.empty
  it "update status when task exists" \(env, db) -> hedgehog do
    (task, newStatus) <- forAll $ (,) <$> genTask <*> genTaskStatus
    event <- run env do
      cleanInMemoryDb db
      saveInMemory db task
      taskHandle $ UpdateStatus (taskId task) newStatus
    let expected = task {taskStatus = newStatus}
    event === TaskEvent (Task.Updated expected)
    updatedTask <- Map.lookup (taskId task) <$> readIORef db
    updatedTask === Just expected

run :: (MonadIO m) => TestEnvApp -> TestApp a -> m a
run env = liftIO . usingReaderT env . unApp

withEnv :: IO (TestEnvApp, InMemoryDb)
withEnv = do
  db <- newIORef Map.empty
  env <- mkEnv db
  pure (env, db)
