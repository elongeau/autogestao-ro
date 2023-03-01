module Autogestao.App.Print (
  printReport,
) where

import Autogestao.Core.Task (
  Task (Todo),
  TaskId (TaskId),
 )

printReport :: [Task] -> Text
printReport = \case
  [] -> "no tasks found"
  tasks -> unlines . fmap fmt $ tasks

fmt :: Task -> Text
fmt (Todo (TaskId taskId) desc) = show taskId <> " - " <> desc
