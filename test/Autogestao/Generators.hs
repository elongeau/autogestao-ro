module Autogestao.Generators where

import Autogestao.Core.Task (Task (Todo, taskId), TaskId (TaskId))
import Data.List (nubBy)
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genMany :: Gen.Gen a -> Gen.Gen [a]
genMany = Gen.list (Range.constant 1 10)

genZeroOrMany :: Gen.Gen a -> Gen.Gen [a]
genZeroOrMany = Gen.list (Range.constant 0 10)

genTaskId :: Gen.Gen TaskId
genTaskId = TaskId <$> Gen.int (Range.constant 1 100)

genText :: Gen.Gen Text
genText = Gen.text (Range.constant 3 10) Gen.alpha

genTask :: Gen.Gen Task
genTask = Todo <$> genTaskId <*> genText

distinctTasks :: [Task] -> [Task]
distinctTasks = nubBy (\t1 t2 -> taskId t1 == taskId t2)
