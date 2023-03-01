module Autogestao.Infra.UserInputParserSpec where

import Autogestao.App.UserInput (UserInput (UserQueryAllTasks, UserTaskCommand))
import Autogestao.Core.Task (TaskId (unId))
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd))
import Autogestao.Generators
import Autogestao.Infra.UserInputParser (
  parser,
 )
import Options.Applicative
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )
import Test.Hspec.Hedgehog

run :: [Text] -> Maybe UserInput
run = getParseResult . execParserPure defaultPrefs (info parser fullDesc) . fmap toString

spec :: Spec
spec = do
  describe "Command line arguments parsing" do
    describe "Adding a task" do
      it "returns an 'Add' command with the same text" do
        hedgehog do
          inputs <- forAll $ genMany genText
          run ("add" : inputs) === Just (UserTaskCommand . TaskCmd $ Task.Add $ unwords inputs)
    describe "List tasks" do
      it "returns a 'List' query" do
        run ["ls"] `shouldBe` Just UserQueryAllTasks
    describe "update task description" do
      it "returns an 'Update' command with ID and new description" do
        hedgehog do
          taskId <- forAll genTaskId
          inputs <- forAll $ genMany genText
          run ("update" : show (unId taskId) : inputs) === Just (UserTaskCommand . TaskCmd $ Task.Update taskId (unwords inputs))
