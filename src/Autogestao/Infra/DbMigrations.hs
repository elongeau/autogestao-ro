module Autogestao.Infra.DbMigrations where

import Database.Migrant
import Database.Migrant.Driver.Sqlite ()
import Database.SQLite.Simple

runMigrations :: Connection -> IO ()
runMigrations = migrate migrations up down

migrations :: [MigrationName]
migrations =
  [ "create-task-table"
  ]

up :: MigrationName -> Connection -> IO ()
up name conn = case name of
  "create-task-table" -> execute_ conn "CREATE TABLE tasks (id INTEGER PRIMARY KEY, description TEXT)"
  _ -> pass

down :: MigrationName -> Connection -> IO ()
down name conn = case name of
  "create-task-table" -> execute_ conn "DROP TABLE tasks "
  _ -> pass
