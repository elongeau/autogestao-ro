module Autogestao.Core.Repository where

import Autogestao.Core.Event (Event (TaskEvent))
import Autogestao.Core.Event qualified as E
import Autogestao.Core.Has (Has, grab)
import Autogestao.Core.Log (HasLog, logInfo)
import Autogestao.Core.Task (Task, TaskId)
import Autogestao.Core.Task qualified as Task

data Repository m = Repository
  { repositorySave :: Task -> m ()
  , repositoryFind :: forall a. Query a -> m a
  }

data Query a where
  FindAllTasks :: Query [Task]
  FindNextId :: Query TaskId
  FindById :: TaskId -> Query (Maybe Task)

type HasRepository env m = (MonadReader env m, Has (Repository m) env)

saveOne :: forall env m. (HasRepository env m, HasLog env m) => Task -> m ()
saveOne task = do
  logInfo "save task"
  grab @(Repository m) >>= \repository -> repository.repositorySave task

runQuery :: forall env m a. (HasRepository env m) => Query a -> m a
runQuery query = grab @(Repository m) >>= \(Repository {..}) -> repositoryFind query

save :: forall env m. (HasRepository env m, HasLog env m) => E.Event -> m ()
save = \case
  TaskEvent (Task.Created task) -> saveOne task
  TaskEvent (Task.Updated task) -> saveOne task
  TaskEvent (Task.NotFound _) -> pass
