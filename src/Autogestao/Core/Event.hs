module Autogestao.Core.Event where

import Autogestao.Core.Task qualified as Task

newtype Event = TaskEvent Task.Event
  deriving stock (Show, Eq)
