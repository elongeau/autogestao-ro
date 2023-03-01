module Autogestao.Core.Log (Log (Log, log), LogLevel (..), logInfo, HasLog) where

import Autogestao.Core.Has (Has, grab)

newtype Log m = Log
  { log :: LogLevel -> Text -> m ()
  }

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Show)
type HasLog env m = (MonadReader env m, Has (Log m) env)

logInfo :: (HasLog env m) => Text -> m ()
logInfo = appLog Info

appLog :: forall env m. (HasLog env m) => LogLevel -> Text -> m ()
appLog lvl msg =
  grab @(Log m) >>= \Log {..} -> log lvl msg
