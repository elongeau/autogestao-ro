module Autogestao.App.SqliteRepository (
  mkSqliteRepository,
)
where

import Autogestao.Core.Repository (Query (FindAllTasks, FindById, FindNextId), Repository (Repository, repositoryFind, repositorySave))
import Autogestao.Core.Task (
  Task (..),
  TaskId (..),
 )
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Safe (headMay)

mkSqliteRepository :: forall m. MonadIO m => SQL.Connection -> IO (Repository m)
mkSqliteRepository conn =
  pure $
    Repository
      { repositorySave = liftIO . save conn
      , repositoryFind = liftIO . runQuery conn
      }

toTask :: (Int64, Text) -> Task
toTask (i, desc) = Todo (TaskId $ fromIntegral i) desc

fromTask :: Task -> [NamedParam]
fromTask = \case
  Todo (TaskId i) desc -> [":id" := i, ":desc" := desc]

save :: SQL.Connection -> Task -> IO ()
save conn = SQL.executeNamed conn "INSERT INTO tasks (id, description) VALUES (:id, :desc) ON CONFLICT(id) DO UPDATE SET description = :desc" . fromTask

runQuery :: forall a. SQL.Connection -> Query a -> IO a
runQuery conn = \case
  FindAllTasks -> fmap toTask <$> SQL.query_ conn "SELECT id, description FROM tasks ORDER BY id"
  FindNextId -> do
    ids <- SQL.query_ conn "SELECT MAX(id) from tasks" :: IO [SQL.Only (Maybe Int)]
    let nextId = TaskId . fromMaybe 1 . headMay . fmap (maybe 1 (+ 1) . SQL.fromOnly) $ ids
    pure nextId
  FindById taskIdToFind -> do
    tasks <- fmap toTask <$> SQL.query conn "SELECT id, description from tasks WHERE id = ?" (SQL.Only taskIdToFind.unId)
    pure . headMay $ tasks
