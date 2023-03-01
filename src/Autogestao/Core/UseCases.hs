module Autogestao.Core.UseCases where

import Autogestao.Core.Event (Event (TaskEvent))
import Autogestao.Core.Repository (HasRepository, save)
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases.TaskUseCases qualified as Task

newtype Command = TaskCmd Task.Command
  deriving stock (Show, Eq)

decide :: forall env m. (HasRepository env m) => Command -> m Event
decide = \case
  TaskCmd cmd -> do
    event <- TaskEvent <$> Task.decide cmd
    save event
    pure event
