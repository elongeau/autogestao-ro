module Autogestao.Infra.DbMigrations where

import Database.Migrant
import Database.Migrant.Driver.Sqlite ()
import Database.SQLite.Simple

runMigrations :: Connection -> IO ()
runMigrations = migrate migrations up down

migrations :: [MigrationName]
migrations =
  [ "create-task-table"
  , "add-task-status"
  , "add-task-tags"
  , "add-task-notes"
  ]

up :: MigrationName -> Connection -> IO ()
up name conn = case name of
  "create-task-table" -> execute_ conn "CREATE TABLE tasks (id INTEGER PRIMARY KEY, description TEXT)"
  "add-task-status" -> execute_ conn "ALTER TABLE tasks ADD COLUMN status TEXT CHECK (status IN ('Todo', 'Ongoing', 'Next', 'Canceled', 'Done'))"
  "add-task-tags" -> execute_ conn "CREATE TABLE tags (task_id INTEGER, name TEXT, FOREIGN KEY(task_id) REFERENCES tasks(id))"
  "add-task-notes" -> execute_ conn "ALTER TABLE tasks ADD COLUMN notes TEXT"
  _ -> pass

down :: MigrationName -> Connection -> IO ()
down name conn = case name of
  "create-task-table" -> execute_ conn "DROP TABLE tasks "
  "add-task-status" -> execute_ conn "ALTER TABLE tasks DROP COLUMN status "
  "add-task-tags" -> execute_ conn "DROP TABLE tags"
  "add-task-notes" -> execute_ conn "ALTER TABLE tasks DROP COLUMN notes"
  _ -> pass
