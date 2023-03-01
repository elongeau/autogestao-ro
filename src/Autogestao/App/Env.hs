module Autogestao.App.Env where

import Autogestao.Config (Out)
import Autogestao.Core.Has (Field (Field), Has)
import Autogestao.Core.Log (Log)
import Autogestao.Core.Repository (Repository)
import Database.SQLite.Simple qualified as SQL

data Env m = Env
  { envRepository :: Repository m
  , envLogger :: Log m
  }
  deriving (Has (Repository m)) via Field "envRepository" (Env m)
  deriving (Has (Log m)) via Field "envLogger" (Env m)

data EnvInput = EnvInput
  { db :: SQL.Connection
  , logOut :: !Out
  }
