module Autogestao.Envs (InMemoryDb, noLogger, mkEnv, TestEnv (..), TestApp (..), TestEnvApp) where

import Autogestao.Core.Has (Field (Field), Has)
import Autogestao.Core.InMemoryRepository (
  InMemoryDb,
  mkInMemory,
 )
import Autogestao.Core.Log (Log (Log))
import Autogestao.Core.Repository
import Autogestao.Core.Task (Task, TaskId (..))

noLogger :: Log TestApp
noLogger = Log $ \_ _ -> pass

type TestEnvApp = TestEnv TestApp
data TestEnv m = TestEnv
  { repository :: Repository m
  , db :: IORef (Map TaskId Task)
  , log :: Log m
  }
  deriving (Has (Repository m)) via Field "repository" (TestEnv m)
  deriving (Has (Log m)) via Field "log" (TestEnv m)

newtype TestApp a = TestApp {unApp :: ReaderT TestEnvApp IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnvApp, MonadFail)

mkEnv :: InMemoryDb -> IO TestEnvApp
mkEnv db = do
  let repository = mkInMemory db
  let log = noLogger
  pure $ TestEnv repository db log
