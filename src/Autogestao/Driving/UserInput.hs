module Autogestao.Driving.UserInput where

import Autogestao.Core.Repository (Criteria, Sort)
import Autogestao.Core.Task (TaskId)
import Autogestao.Core.UseCases

data UserInput
  = UserTaskCommand Command
  | UserQueryTasks Criteria Sort
  | OpenEditor EditorAction
  deriving stock (Show, Eq)

data EditorAction = New | Edit TaskId
  deriving stock (Show, Eq)
