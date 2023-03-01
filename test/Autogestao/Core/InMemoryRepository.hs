module Autogestao.Core.InMemoryRepository where

import Autogestao.Core.Repository (Query (..), Repository (Repository, repositoryFind, repositorySave))
import Autogestao.Core.Task (Task (taskId), TaskId (TaskId, unId))
import Data.Map.Strict (elems, insert, keys, lookup)
import Data.Map.Strict qualified as Map
import Safe (maximumMay)

type InMemoryDb = IORef (Map TaskId Task)

mkInMemory :: forall m. MonadIO m => InMemoryDb -> Repository m
mkInMemory db =
  Repository
    { repositorySave = saveInMemory db
    , repositoryFind = \case
        FindAllTasks -> elems <$> readIORef db
        FindNextId -> do
          taskIds <- keys <$> readIORef db
          taskIds
            & fmap unId
            & maximumMay
            & maybe 1 (+ 1)
            & TaskId
            & pure
        FindById taskId' -> lookup taskId' <$> readIORef db
    }

cleanInMemoryDb :: MonadIO m => InMemoryDb -> m ()
cleanInMemoryDb db = writeIORef db Map.empty

saveInMemory :: MonadIO m => InMemoryDb -> Task -> m ()
saveInMemory db task = modifyIORef' db $ insert (taskId task) task
