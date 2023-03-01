module Autogestao.App.ReportSpec where

import Autogestao.App.Report
import Autogestao.Core.Event qualified as E
import Autogestao.Core.Task (Event (Created, NotFound, Updated), Task (Todo, taskId), TaskId (TaskId, unId))
import Autogestao.Generators (genTask, genTaskId)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = describe "converted to text" do
  describe "Events" do
    describe "Task events" do
      it "represent a task creation" $ hedgehog do
        task <- forAll genTask
        let report = EventReport $ E.TaskEvent $ Created task
        toText report === "Task " <> (show . unId . taskId $ task) <> " created"
      it "represent a task update" $ hedgehog do
        task <- forAll genTask
        let report = EventReport $ E.TaskEvent $ Updated task
        toText report === "Task " <> (show . unId . taskId $ task) <> " updated"
      it "represent a task not found" $ hedgehog do
        taskId' <- forAll genTaskId
        let report = EventReport $ E.TaskEvent $ NotFound taskId'
        toText report === "Task " <> (show . unId $ taskId') <> " not found"
  describe "Listings" do
    describe "Task listings" do
      it "represent empty listing" do
        let noTasks = ListingReport $ TaskListing []
        toText noTasks `shouldBe` "no task found"
      it "represent not empty listing" do
        let noTasks = ListingReport $ TaskListing [Todo (TaskId 1) "do something", Todo (TaskId 42) "find the answer"]
        toText noTasks
          `shouldBe` unlines
            [ "1 - do something"
            , "42 - find the answer"
            ]
