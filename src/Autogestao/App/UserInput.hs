module Autogestao.App.UserInput where

import Autogestao.Core.UseCases

data UserInput
  = UserTaskCommand Command
  | UserQueryAllTasks
  deriving stock (Show, Eq)
