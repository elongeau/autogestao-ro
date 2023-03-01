module Autogestao.Core.UseCases.TaskUseCases where

import Autogestao.Core.Repository (
  HasRepository,
  Query (FindById, FindNextId),
  runQuery,
 )
import Autogestao.Core.Task (
  Command (Add, Update, UpdateStatus),
  Event (Created, NotFound, Updated),
  Task (Task, taskDescription, taskNotes, taskStatus, taskTags),
  TaskId,
  TaskNotes (NoNotes, Notes),
  TaskStatus (Todo),
  TaskTag (TaskTag, unTag),
 )
import Data.List (dropWhileEnd)
import Data.Text (null, strip, toLower)
import Prelude hiding (null)

decide :: (HasRepository env m) => Command -> m Event
decide = \case
  Add description tags notes -> do
    nextId <- runQuery FindNextId
    pure $ Created $ Task nextId description Todo (fmap (TaskTag . toLower . unTag) tags) (trimNotes notes)
  Update taskId desc tags notes ->
    updateTask taskId \task ->
      Updated
        task
          { taskDescription = desc
          , taskTags = fmap (TaskTag . toLower . unTag) tags
          , taskNotes = trimNotes notes
          }
  UpdateStatus taskId newStatus -> do
    updateTask taskId \task -> Updated task {taskStatus = newStatus}

updateTask :: (HasRepository env m) => TaskId -> (Task -> Event) -> m Event
updateTask taskId f = do
  maybeTask <- runQuery $ FindById taskId
  pure $ maybe (NotFound taskId) f maybeTask

trimNotes :: TaskNotes -> TaskNotes
trimNotes = \case
  NoNotes -> NoNotes
  Notes notes -> Notes . trimLines $ notes

trimLines :: Text -> Text
trimLines = strip . unlines . dropWhile null . dropWhileEnd null . lines
