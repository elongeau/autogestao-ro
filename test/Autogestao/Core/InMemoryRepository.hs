module Autogestao.Core.InMemoryRepository where

import Autogestao.Core.Repository (Criteria (AllOf, None, StatusIn, Tags), Query (..), Repository (Repository, repositoryFind, repositorySave), Sort (NoSort, SortById, SortByStatus))
import Autogestao.Core.Task (Task (taskId, taskStatus, taskTags), TaskId (TaskId, unId))
import Data.List (intersect)
import Data.Map.Strict (elems, insert, keys, lookup)
import Data.Map.Strict qualified as Map
import Safe (maximumMay)

type InMemoryDb = IORef (Map TaskId Task)

mkInMemory :: forall m. MonadIO m => InMemoryDb -> Repository m
mkInMemory db =
  Repository
    { repositorySave = saveInMemory db
    , repositoryFind = \case
        FindBy criteria sorting -> sortFn sorting . findByCriteria criteria . elems <$> readIORef db
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

findByCriteria :: Criteria -> [Task] -> [Task]
findByCriteria = \case
  None -> identity
  (StatusIn statuses) -> (filter \task -> taskStatus task `elem` statuses)
  (Tags tags) -> (filter \task -> [] /= (taskTags task `intersect` tags))
  AllOf criterias -> go criterias
    where
      go :: [Criteria] -> [Task] -> [Task]
      go [] tasks = tasks
      go (criteria : rest) tasks = go rest $ findByCriteria criteria tasks

cleanInMemoryDb :: MonadIO m => InMemoryDb -> m ()
cleanInMemoryDb db = writeIORef db Map.empty

saveInMemory :: MonadIO m => InMemoryDb -> Task -> m ()
saveInMemory db task = modifyIORef' db $ insert (taskId task) task

sortFn :: Sort -> [Task] -> [Task]
sortFn sorting = sortBy comparingFn
  where
    comparingFn = case sorting of
      SortById -> compare `on` taskId
      SortByStatus -> compare `on` taskStatus
      NoSort -> \_ _ -> EQ
