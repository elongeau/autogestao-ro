module Autogestao.App where

import Autogestao.Driven.Log (mkLog)
import Autogestao.Driven.Monad (AppEnv)
import Autogestao.Driven.SqliteRepository (mkSqliteRepository)
import Autogestao.Driving.Env (Env (Env), EnvInput (..))

mkEnv :: EnvInput -> IO AppEnv
mkEnv EnvInput {..} = do
  let envLogger = mkLog logOut
  envRepository <- mkSqliteRepository db
  pure $ Env envRepository envLogger
