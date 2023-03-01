module Autogestao.App.Report (Report (..), Listing (..)) where

import Autogestao.Core.Event qualified as E
import Autogestao.Core.Task (Event (Created, NotFound, Updated), Task (Todo), TaskId (TaskId, unId))

data Report
  = EventReport E.Event
  | ListingReport Listing
  deriving stock (Show, Eq)

newtype Listing = TaskListing [Task]
  deriving stock (Show, Eq)

instance ToText Report where
  toText = \case
    EventReport event -> eventToText event
    ListingReport listing -> listingToText listing

eventToText :: E.Event -> Text
eventToText = \case
  E.TaskEvent event -> case event of
    Created (Todo taskId _) -> "Task " <> show taskId.unId <> " created"
    Updated (Todo taskId _) -> "Task " <> show taskId.unId <> " updated"
    NotFound taskId -> "Task " <> show taskId.unId <> " not found"

listingToText :: Listing -> Text
listingToText = \case
  TaskListing tasks -> case tasks of
    [] -> "no task found"
    tasks' -> unlines $ fmap taskToText tasks'

taskToText :: Task -> Text
taskToText (Todo (TaskId i) desc) = show i <> " - " <> desc
