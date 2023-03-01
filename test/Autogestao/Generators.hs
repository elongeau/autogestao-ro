module Autogestao.Generators where

import Autogestao.Core.Task (
  Task (Task, taskId),
  TaskDescription (Desc),
  TaskId (TaskId),
  TaskNotes (NoNotes, Notes),
  TaskStatus,
  TaskTag (TaskTag, unTag),
 )
import Data.List (nubBy)
import Data.Text (toLower)
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

genTaskDesc :: Gen.Gen TaskDescription
genTaskDesc = Desc <$> genText

genTask :: Gen.Gen Task
genTask = Task <$> genTaskId <*> genTaskDesc <*> genTaskStatus <*> genTags <*> pure NoNotes

genTags :: Gen.Gen [TaskTag]
genTags = genMany genTag <&> \tags -> sortBy (compare `on` unTag) tags

genNotes :: Gen.Gen TaskNotes
genNotes =
  Gen.choice
    [ pure NoNotes
    , Notes <$> genText
    ]

genTag :: Gen.Gen TaskTag
genTag = TaskTag . toLower <$> genText

genTaskStatus :: Gen.Gen TaskStatus
genTaskStatus = Gen.enumBounded

distinctTasks :: [Task] -> [Task]
distinctTasks = nubBy (\t1 t2 -> taskId t1 == taskId t2)

tupled3 :: Gen.Gen a -> Gen.Gen b -> Gen.Gen c -> Gen.Gen (a, b, c)
tupled3 a b c = (,,) <$> a <*> b <*> c
