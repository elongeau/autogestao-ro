module Autogestao.App.Log where

import Autogestao.Config (
  Out (Console, File),
 )
import Autogestao.Core.Log (Log (Log), LogLevel)
import Data.Text (
  toUpper,
 )
import Data.Time (
  getCurrentTime,
 )

mkLog :: (MonadIO m) => Out -> Log m
mkLog = Log . log'

log' :: (MonadIO m) => Out -> LogLevel -> Text -> m ()
log' out =
  let write = case out of
        Console -> putTextLn
        File folder -> appendFileText (folder <> "/.autogestao/logs/log.txt")
   in \lvl msg -> do
        now <- liftIO getCurrentTime
        write $ "[" <> show now <> "] [" <> toUpper (show lvl) <> "] " <> msg
