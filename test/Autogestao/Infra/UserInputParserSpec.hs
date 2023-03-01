{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Autogestao.Infra.UserInputParserSpec where

import Autogestao.Core.Repository (Criteria (AllOf, StatusIn, Tags), Sort (SortById, SortByStatus), allTasks, defaultCriteria)
import Autogestao.Core.Task (
  Task (taskDescription, taskTags),
  TaskDescription (Desc, unDesc),
  TaskId (TaskId, unId),
  TaskNotes (NoNotes, Notes),
  TaskStatus (Canceled, Done, Next, Ongoing, Todo),
  TaskTag (TaskTag, unTag),
 )
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd))
import Autogestao.Driving.UserInput (
  EditorAction (Edit, New),
  UserInput (OpenEditor, UserQueryTasks, UserTaskCommand),
 )
import Autogestao.Generators
import Autogestao.Infra.UserInputParser (
  FromEditor (FromEditor, NoContent),
  parser,
  readEditorContent,
 )
import Data.Text (pack)
import Data.Text qualified as Text
import Options.Applicative hiding (Parser, many, some)
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, string)
import Prelude hiding (many, some)

run :: [Text] -> Maybe UserInput
run = getParseResult . execParserPure defaultPrefs (info parser fullDesc) . fmap toString

spec :: Spec
spec = do
  describe "Command line arguments parsing" do
    describe "Create a new task from editor" do
      it "returns an 'OpenEditor New'" do
        run ["new"] `shouldBe` Just (OpenEditor New)
    describe "update status" do
      it "returns an UpdateStatus command with ID and new status" $ hedgehog do
        taskId <- forAll genTaskId
        newStatus <- forAll genTaskStatus
        let cmd = case newStatus of
              Todo -> "todo"
              Next -> "next"
              Ongoing -> "start"
              Done -> "done"
              Canceled -> "cancel"
        let args = [cmd, show . unId $ taskId]
        run args === Just (UserTaskCommand . TaskCmd $ Task.UpdateStatus taskId newStatus)
    describe "edit task" do
      it "returns an OpenEditor command" do
        run ["edit", "1"] `shouldBe` Just (OpenEditor . Edit $ TaskId 1)
    editorSpec
    listSpec

listSpec :: Spec
listSpec = describe "List tasks" do
  it "returns a 'List active tasks' with a sort by status query" do
    run ["ls"] `shouldBe` Just (UserQueryTasks defaultCriteria SortByStatus)
  it "returns a 'List all tasks' query" do
    run ["ls", "-a"] `shouldBe` Just (UserQueryTasks allTasks SortByStatus)
    run ["ls", "--all"] `shouldBe` Just (UserQueryTasks allTasks SortByStatus)
  it "returns a 'List tasks by status' query" $ hedgehog do
    status <- forAll genTaskStatus
    let (shortFlag, longFlag) = case status of
          Todo -> ("t", "todo")
          Next -> ("n", "next")
          Ongoing -> ("o", "ongoing")
          Done -> ("d", "done")
          Canceled -> ("c", "canceled")
    run ["ls", "-" <> shortFlag] === Just (UserQueryTasks (StatusIn [status]) SortByStatus)
    run ["ls", "--" <> longFlag] === Just (UserQueryTasks (StatusIn [status]) SortByStatus)
  it "accept many status" do
    run ["ls", "-n", "--done"] `shouldBe` Just (UserQueryTasks (StatusIn [Next, Done]) SortByStatus)
  it "can sort by task ID" do
    run ["ls", "-s", "id"] `shouldBe` Just (UserQueryTasks defaultCriteria SortById)
    run ["ls", "--sort", "id"] `shouldBe` Just (UserQueryTasks defaultCriteria SortById)
  it "returns a 'List tasks by tags' query" do
    run ["ls", "-p", "foo"] `shouldBe` Just (UserQueryTasks (Tags [TaskTag "foo"]) SortByStatus)
  it "accept mixed argument" do
    run ["ls", "-p", "foo", "-o"] `shouldBe` Just (UserQueryTasks (AllOf [Tags [TaskTag "foo"], StatusIn [Ongoing]]) SortByStatus)
    run ["ls", "-n", "-o"] `shouldBe` Just (UserQueryTasks (StatusIn [Ongoing, Next]) SortByStatus)

editorSpec :: Spec
editorSpec = describe "reading content from editor" do
  it "parse content" do
    let content = "foo bar #qux 123 éàîoï foo's \"quote\" #tag ,./<>?;\\:|!@$%&*()"
    let result = readEditorContent content
    result `shouldBe` FromEditor (Desc "foo bar 123 éàîoï foo's \"quote\" ,./<>?;\\:|!@$%&*()") [TaskTag "qux", TaskTag "tag"] NoNotes
  it "handle empty content" do
    let result = readEditorContent ""
    result `shouldBe` NoContent
  it "parse multiline content" do
    let content = unlines ["header #tag", "", "some notes"]
    let result = readEditorContent content
    result `shouldBe` FromEditor (Desc "header") [TaskTag "tag"] (Notes . unlines $ ["", "some notes"])
