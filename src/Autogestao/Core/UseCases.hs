module Autogestao.Core.UseCases where

import Autogestao.Core.Event (Event (TaskEvent))
import Autogestao.Core.Log (HasLog)
import Autogestao.Core.Repository (HasRepository, save)
import Autogestao.Core.Task qualified as Task
import Autogestao.Core.UseCases.TaskUseCases qualified as Task

newtype Command = TaskCmd Task.Command
  deriving stock (Show, Eq)

handle :: forall env m. (HasRepository env m, HasLog env m) => Command -> m Event
handle = \case
  TaskCmd cmd -> do
    event <- TaskEvent <$> Task.handle cmd
    save event
    pure event
