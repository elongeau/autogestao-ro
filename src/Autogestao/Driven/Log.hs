module Autogestao.Driven.Log (mkLog, LogTarget (..)) where

import Autogestao.Core.Log (Log (Log), LogLevel)
import Data.Text (
  toUpper,
 )
import Data.Time (
  getCurrentTime,
 )

newtype LogTarget = LogFolder FilePath
  deriving stock (Show, Eq)

mkLog :: (MonadIO m) => LogTarget -> Log m
mkLog = Log . log'

log' :: (MonadIO m) => LogTarget -> LogLevel -> Text -> m ()
log' (LogFolder folder) =
  let write txt = appendFileText (folder <> "/log.txt") $ unlines [txt]
   in \lvl msg -> do
        now <- liftIO getCurrentTime
        write $ "[" <> show now <> "] [" <> toUpper (show lvl) <> "] " <> msg
