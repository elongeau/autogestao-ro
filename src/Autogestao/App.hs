module Autogestao.App where

import Autogestao.App.Env (Env (Env), EnvInput (..))
import Autogestao.App.Log (mkLog)
import Autogestao.App.Monad (AppEnv)
import Autogestao.App.SqliteRepository (mkSqliteRepository)

mkEnv :: EnvInput -> IO AppEnv
mkEnv EnvInput {..} = do
  let envLogger = mkLog logOut
  envRepository <- mkSqliteRepository db
  pure $ Env envRepository envLogger
