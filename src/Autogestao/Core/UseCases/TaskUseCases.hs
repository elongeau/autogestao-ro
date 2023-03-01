module Autogestao.Core.UseCases.TaskUseCases where

import Autogestao.Core.Log (HasLog)
import Autogestao.Core.Repository (
  HasRepository,
  Query (FindById, FindNextId),
  runQuery,
 )
import Autogestao.Core.Task (Command (Add, Update), Event (Created, NotFound, Updated), Task (Todo, taskDescription))

handle :: (HasRepository env m, HasLog env m) => Command -> m Event
handle = \case
  Add description -> do
    nextId <- runQuery FindNextId
    pure $ Created $ Todo nextId description
  Update taskId desc -> do
    maybeTask <- runQuery $ FindById taskId
    let update task = Updated $ task {taskDescription = desc}
    pure $ maybe (NotFound taskId) update maybeTask
