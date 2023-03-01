module Autogestao.Driven.SqliteRepository (
  mkSqliteRepository,
)
where

import Autogestao.Core.Repository (
  Criteria (AllOf, None, StatusIn, Tags),
  Query (FindBy, FindById, FindNextId),
  Repository (Repository, repositoryFind, repositorySave),
  Sort (NoSort, SortById, SortByStatus),
 )
import Autogestao.Core.Task (
  Task (..),
  TaskDescription (Desc),
  TaskId (..),
  TaskNotes (NoNotes, Notes),
  TaskTag (TaskTag, unTag),
 )
import Data.List (singleton)
import Database.SQLite.Simple (NamedParam ((:=)), field)
import Database.SQLite.Simple qualified as SQL
import Relude.Unsafe (read)
import Safe (headMay)

mkSqliteRepository :: forall m. MonadIO m => SQL.Connection -> IO (Repository m)
mkSqliteRepository conn =
  pure $
    Repository
      { repositorySave = liftIO . save conn
      , repositoryFind = liftIO . runQuery conn
      }

fromTask :: Task -> [NamedParam]
fromTask (Task (TaskId taskId) (Desc desc) status _ notes) =
  [ ":id" := taskId
  , ":desc" := desc
  , ":status" := show @Text status
  , ":notes" := case notes of
      NoNotes -> Nothing
      Notes notes' -> Just notes'
  ]

fromTags :: Task -> [[NamedParam]]
fromTags task = taskTags task <&> \tag -> [":id" := (unId . taskId $ task), ":name" := unTag tag]

save :: SQL.Connection -> Task -> IO ()
save conn task = do
  SQL.executeNamed conn "INSERT INTO tasks (id, description, status, notes) VALUES (:id, :desc, :status, :notes) ON CONFLICT(id) DO UPDATE SET description = :desc, status = :status, notes = :notes" $ fromTask task
  SQL.executeNamed conn "DELETE FROM tags WHERE task_id = :id" [":id" := (unId . taskId $ task)]
  forM_ (fromTags task) $ SQL.executeNamed conn "INSERT INTO tags (task_id, name) VALUES (:id, :name)"

data DbTask = DbTask Int Text Text (Maybe Text) (Maybe Text) deriving stock (Show)
instance SQL.FromRow DbTask where
  fromRow = DbTask <$> field <*> field <*> field <*> field <*> field

runQuery :: forall a. SQL.Connection -> Query a -> IO a
runQuery conn = \case
  FindBy criteria sorting -> do
    let query = selectTask (whereAnd . criteriaQ $ criteria) sorting
    rawTasks <- SQL.query_ conn query :: IO [DbTask]
    pure . toTasks $ rawTasks
  FindNextId -> do
    ids <- SQL.query_ conn "SELECT MAX(id) from tasks" :: IO [SQL.Only (Maybe Int)]
    let nextId = TaskId . fromMaybe 1 . headMay . fmap (maybe 1 (+ 1) . SQL.fromOnly) $ ids
    pure nextId
  FindById taskIdToFind -> do
    rawTasks <- SQL.query conn (selectTask "WHERE id = ?" NoSort) (SQL.Only taskIdToFind.unId)
    pure . headMay . toTasks $ rawTasks

whereAnd :: [Text] -> Text
whereAnd = \case
  [] -> ""
  queries -> "WHERE " <> unwords (intersperse " AND " queries)

criteriaQ :: Criteria -> [Text]
criteriaQ = \case
  None -> []
  StatusIn statuses ->
    let inStatus = toText (intercalate "," . fmap (\s -> "'" <> show s <> "'") $ statuses)
     in ["status IN ( " <> inStatus <> ")"]
  Tags tags ->
    let inTags = toText (intercalate "," (fmap ((\s -> "'" <> s <> "'") . toString . unTag) tags))
     in ["tg1.name IN ( " <> inTags <> ")"]
  AllOf criterias -> criterias >>= criteriaQ

toTasks :: [DbTask] -> [Task]
toTasks = foldl' combine []

combine :: [Task] -> DbTask -> [Task]
combine acc (DbTask i desc status tag maybeNotes) = updTasks
  where
    updTasks :: [Task]
    updTasks =
      let (xs, ys) = break (\task -> i == (unId . taskId $ task)) acc
          notes = maybe NoNotes Notes maybeNotes
          yys = case ys of
            [] -> singleton $ Task (TaskId $ fromIntegral i) (Desc desc) (read . toString $ status) fromMaybeTag notes
            (task : rest) -> (task {taskTags = taskTags task ++ fromMaybeTag}) : rest
       in xs ++ yys
    fromMaybeTag :: [TaskTag]
    fromMaybeTag = fmap TaskTag . maybeToList $ tag

selectTask :: Text -> Sort -> SQL.Query
selectTask condition sorting = SQL.Query ("SELECT DISTINCT id, description, status, tg2.name, notes from tasks t LEFT JOIN tags tg1 ON t.id = tg1.task_id LEFT JOIN tags tg2 ON t.id = tg2.task_id " <> condition <> " " <> sortClause sorting)

sortClause :: Sort -> Text
sortClause = \case
  SortById -> "ORDER BY id"
  SortByStatus ->
    "ORDER BY CASE status "
      <> "WHEN 'Canceled' THEN 0 "
      <> "WHEN 'Done' THEN 1 "
      <> "WHEN 'Todo' THEN 2 "
      <> "WHEN 'Next' THEN 3 "
      <> "WHEN 'Ongoing' THEN 4 "
      <> "END DESC"
  NoSort -> ""
