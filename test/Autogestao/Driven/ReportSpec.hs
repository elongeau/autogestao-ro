module Autogestao.Driven.ReportSpec where

import Autogestao.Core.Event qualified as E
import Autogestao.Core.Task (Event (Created, NotFound, Updated), Task (Task, taskId), TaskId (TaskId, unId), TaskNotes (NoNotes), TaskStatus (..))
import Autogestao.Driven.Report
import Autogestao.Generators (genTask, genTaskId)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
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
        let report =
              toString . toText . ListingReport $
                TaskListing
                  [ Task (TaskId 1) "todo" Todo ["foo"] NoNotes
                  , Task (TaskId 2) "uselessly long description of a task" Next ["bar", "qux"] NoNotes
                  , Task (TaskId 3) "ongoing" Ongoing [] NoNotes
                  , Task (TaskId 4) "finished" Done [] NoNotes
                  , Task (TaskId 1234) "long ID" Canceled [] NoNotes
                  ]
        defaultGolden "listing-reports" report
      it "has a space between id and description columns" do
        let report = toString . toText . ListingReport $ TaskListing [Task (TaskId 1) "todo" Todo [] NoNotes]
        defaultGolden "listing-reports-space-between-cols" report
    describe "Errors" do
      it "just return the error message" do
        toText (ErrorReport "oups") `shouldBe` "oups"
