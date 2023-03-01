module Autogestao.Infra.UserInputParser (runParser, FromEditor (..), parser, readEditorContent) where

import Autogestao.Core.Repository (Criteria (None, Tags), Sort (SortById, SortByStatus), allTasks, defaultCriteria, statusIn)
import Autogestao.Core.Task (
  TaskDescription (Desc),
  TaskId (TaskId),
  TaskNotes (NoNotes, Notes),
  TaskStatus (..),
  TaskTag (TaskTag),
 )
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases (Command (TaskCmd))
import Autogestao.Driving.UserInput (EditorAction (Edit, New), UserInput (OpenEditor, UserQueryTasks, UserTaskCommand))
import Data.Char qualified as Char (toLower)
import Data.Text (strip)
import Data.Text qualified as Text
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  argument,
  command,
  customExecParser,
  flag,
  fullDesc,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  prefs,
  progDesc,
  short,
  showHelpOnEmpty,
  strOption,
  value,
 )
import Options.Applicative.Builder (
  auto,
 )
import Safe (headMay, tailMay)
import Text.Megaparsec (MonadParsec (eof, takeWhile1P), Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Char (alphaNumChar, char, space)

newCommand :: Parser UserInput
newCommand = pure $ OpenEditor New

editCommand :: Parser UserInput
editCommand = OpenEditor . Edit <$> taskIdParser

taskIdParser :: Parser TaskId
taskIdParser = TaskId <$> argument auto (metavar "task ID")

lsCommand :: Parser UserInput
lsCommand = UserQueryTasks <$> (allStatus <|> inputs) <*> sortParser
  where
    allStatus = flag defaultCriteria allTasks (long "all" <> short 'a' <> help "List all tasks")
    inputs = fmap fold . sequenceA $ (tagInput : statusInputs)
    statusInputs =
      universe @TaskStatus <&> \status ->
        let longMod = long . fmap Char.toLower . show $ status
            shortMod = short . Char.toLower . Text.head . show $ status
            helpMod = help $ "List tasks with status " <> show status
         in flag None (statusIn [status]) $ longMod <> shortMod <> helpMod
    tagInput =
      Tags
        <$> many
          ( strOption
              ( long "tag"
                  <> short 'p'
                  <> metavar "tag"
                  <> help "a tag"
              )
          )
    sortParser :: Parser Sort
    sortParser =
      strOption @Text
        ( long "sort"
            <> short 's'
            <> metavar "STRING"
            <> value "status"
        )
        <&> \case
          "id" -> SortById
          _ -> SortByStatus

updateStatusCommand :: TaskStatus -> Parser UserInput
updateStatusCommand newStatus = UserTaskCommand . TaskCmd <$> updateStatus
  where
    updateStatus :: Parser Task.Command
    updateStatus = Task.UpdateStatus <$> taskIdParser <*> pure newStatus

parser :: Parser UserInput
parser =
  hsubparser
    ( command "ls" (info lsCommand (progDesc "List all tasks"))
        <> fold updateStatusCommands
        <> command "new" (info newCommand (progDesc "Create a task in default editor"))
        <> command "edit" (info editCommand (progDesc "Edit a task in default editor"))
    )

updateStatusCommands :: [Mod CommandFields UserInput]
updateStatusCommands =
  universe @TaskStatus <&> \status ->
    command (cmd status) (info (updateStatusCommand status) (progDesc $ "Set task as " <> show status))
  where
    cmd :: TaskStatus -> String
    cmd = \case
      Todo -> "todo"
      Next -> "next"
      Ongoing -> "start"
      Done -> "done"
      Canceled -> "cancel"

opts :: ParserInfo UserInput
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "a CLI to manage todos"
    )

runParser :: IO UserInput
runParser = customExecParser prefs' opts
  where
    prefs' = prefs showHelpOnEmpty

type ParserC a = Parsec Void Text a
data FromEditor
  = FromEditor TaskDescription [TaskTag] TaskNotes
  | NoContent
  | ParseError Text
  deriving stock (Show, Eq)

data EditorChunk = Word TaskDescription | Tag TaskTag

parseWord :: ParserC EditorChunk
parseWord = do
  desc <- takeWhile1P Nothing (not . (`elem` ['\n', ' ', '#']))
  void space
  pure . Word . Desc . toText $ desc

parseTag :: ParserC EditorChunk
parseTag = do
  void $ char '#'
  tag <- some alphaNumChar
  void space
  pure . Tag . TaskTag . toText $ tag

withDesc :: FromEditor -> TaskDescription -> FromEditor
withDesc fromEditor (Desc description) = case fromEditor of
  (FromEditor (Desc d) tags notes) ->
    let newDesc = Desc . strip $ d <> " " <> description
     in FromEditor newDesc tags notes
  NoContent -> FromEditor (Desc description) [] NoNotes
  err -> err

withTag :: FromEditor -> TaskTag -> FromEditor
withTag fromEditor tag = case fromEditor of
  (FromEditor desc tags notes) ->
    let newTags = tags <> [tag]
     in FromEditor desc newTags notes
  NoContent -> FromEditor (Desc "") [tag] NoNotes
  err -> err

parseEmpty :: ParserC FromEditor
parseEmpty = NoContent <$ eof

parseEditorContent :: ParserC FromEditor
parseEditorContent =
  parseEmpty <|> do
    chunks <- some (parseTag <|> parseWord)
    let f acc = \case
          Word desc -> withDesc acc desc
          Tag tag -> withTag acc tag
    pure $ foldl' f (FromEditor (Desc "") [] NoNotes) chunks

readEditorContent :: Text -> FromEditor
readEditorContent content =
  let contentLines = lines content
      header = fromMaybe "" $ headMay contentLines
      header' = case parse parseEditorContent "" header of
        Right val -> val
        Left errors -> ParseError . toText $ errorBundlePretty errors
      notes = case tailMay contentLines of
        Nothing -> NoNotes
        Just [] -> NoNotes
        Just notes' -> Notes . unlines $ notes'
   in case header' of
        FromEditor desc tags _ -> FromEditor desc tags notes
        other -> other
