module Autogestao.Core.Task (
  TaskId (TaskId, unId),
  Task (..),
  Command (..),
  Event (..),
  TaskDescription (Desc, unDesc),
  TaskNotes (..),
  TaskStatus (..),
  TaskTag (..),
) where

newtype TaskId = TaskId {unId :: Int}
  deriving stock (Show, Eq, Ord)

newtype TaskDescription = Desc {unDesc :: Text}
  deriving stock (Show, Eq)
  deriving newtype (IsString)

data TaskNotes
  = NoNotes
  | Notes Text
  deriving stock (Show, Eq)

newtype TaskTag = TaskTag {unTag :: Text}
  deriving stock (Show, Eq)
  deriving newtype (IsString)

data TaskStatus = Ongoing | Next | Todo | Done | Canceled
  deriving stock (Show, Eq, Enum, Bounded, Read, Ord)

data Task = Task
  { taskId :: TaskId
  , taskDescription :: TaskDescription
  , taskStatus :: TaskStatus
  , taskTags :: [TaskTag]
  , taskNotes :: TaskNotes
  }
  deriving stock (Show, Eq)

data Command
  = Add TaskDescription [TaskTag] TaskNotes
  | Update TaskId TaskDescription [TaskTag] TaskNotes
  | UpdateStatus TaskId TaskStatus
  deriving stock (Show, Eq)

data Event
  = Created Task
  | Updated Task
  | NotFound TaskId
  deriving stock (Show, Eq)
