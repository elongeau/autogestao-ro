module Autogestao.Core.Repository where

import Autogestao.Core.Event (Event (TaskEvent))
import Autogestao.Core.Event qualified as E
import Autogestao.Core.Has (Has, grab)
import Autogestao.Core.Task (Task, TaskId, TaskStatus (Next, Ongoing, Todo), TaskTag)
import Autogestao.Core.Task qualified as Task

data Repository m = Repository
  { repositorySave :: Task -> m ()
  , repositoryFind :: forall a. Query a -> m a
  }

data Query a where
  FindBy :: Criteria -> Sort -> Query [Task]
  FindNextId :: Query TaskId
  FindById :: TaskId -> Query (Maybe Task)

data Sort
  = SortById
  | SortByStatus
  | NoSort
  deriving stock (Show, Eq)

data Criteria = StatusIn [TaskStatus] | Tags [TaskTag] | AllOf [Criteria] | None
  deriving stock (Show, Eq)

statusIn :: [TaskStatus] -> Criteria
statusIn = \case
  [] -> None
  statuses -> StatusIn statuses

instance Semigroup Criteria where
  x <> None = x
  None <> x = x
  (StatusIn []) <> right = right
  s@(StatusIn _) <> AllOf right = AllOf (s : right)
  (StatusIn xs) <> (StatusIn ys) = StatusIn (xs ++ ys)
  s@(StatusIn _) <> Tags [] = s
  s@(StatusIn _) <> right = AllOf [s, right]
  (AllOf []) <> right = right
  (AllOf xs) <> (AllOf ys) = AllOf (xs ++ ys)
  (AllOf xs) <> right = AllOf (xs ++ [right])
  (Tags []) <> right = right
  t@(Tags _) <> AllOf right = AllOf (t : right)
  t@(Tags _) <> right = AllOf [t, right]

instance Monoid Criteria where
  mempty = None

defaultCriteria :: Criteria
defaultCriteria = StatusIn [Todo, Next, Ongoing]

allTasks :: Criteria
allTasks = StatusIn universe

type HasRepository env m = (MonadReader env m, Has (Repository m) env)

saveOne :: forall env m. (HasRepository env m) => Task -> m ()
saveOne task = grab @(Repository m) >>= \repository -> repository.repositorySave task

runQuery :: forall env m a. (HasRepository env m) => Query a -> m a
runQuery query = grab @(Repository m) >>= \(Repository {..}) -> repositoryFind query

save :: forall env m. (HasRepository env m) => E.Event -> m ()
save = \case
  TaskEvent (Task.Created task) -> saveOne task
  TaskEvent (Task.Updated task) -> saveOne task
  TaskEvent (Task.NotFound _) -> pass
