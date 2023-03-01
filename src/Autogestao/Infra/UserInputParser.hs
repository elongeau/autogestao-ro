module Autogestao.Infra.UserInputParser where

import Autogestao.App.UserInput (UserInput (UserQueryAllTasks, UserTaskCommand))
import Autogestao.Core.Task (
  TaskId (TaskId),
 )
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd))
import Data.Text qualified as T
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  command,
  fullDesc,
  helper,
  hsubparser,
  info,
  metavar,
  progDesc,
  str,
 )
import Options.Applicative.Builder (
  auto,
 )

tillEndOfLine :: Parser [Text]
tillEndOfLine = many (argument str (metavar "task description"))

addCommand :: Parser UserInput
addCommand = UserTaskCommand . TaskCmd <$> addParser
  where
    addParser :: Parser Task.Command
    addParser = Task.Add <$> (T.intercalate " " <$> tillEndOfLine)

updateCommand :: Parser UserInput
updateCommand = UserTaskCommand . TaskCmd <$> updateParser
  where
    updateParser :: Parser Task.Command
    updateParser = Task.Update <$> (TaskId <$> argument auto (metavar "task ID")) <*> (T.intercalate " " <$> tillEndOfLine)

lsCommand :: Parser UserInput
lsCommand = pure UserQueryAllTasks

parser :: Parser UserInput
parser =
  hsubparser
    ( command "add" (info addCommand (progDesc "Add a task"))
        <> command "ls" (info lsCommand (progDesc "List all tasks"))
        <> command "update" (info updateCommand (progDesc "Update 'description' of the specified task"))
    )

opts :: ParserInfo UserInput
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "a CLI to manage todos"
    )
