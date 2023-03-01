module Autogestao.Core.Task (
  TaskId (TaskId, unId),
  Task (Todo, taskId, taskDescription),
  Command (..),
  Event (..),
) where

newtype TaskId = TaskId {unId :: Int}
  deriving stock (Show, Eq, Ord)

data Task = Todo
  { taskId :: TaskId
  , taskDescription :: Text
  }
  deriving stock (Show, Eq)

data Command
  = Add Text
  | Update TaskId Text
  deriving stock (Show, Eq)

data Event
  = Created Task
  | Updated Task
  | NotFound TaskId
  deriving stock (Show, Eq)
