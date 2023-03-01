{-# OPTIONS_GHC -Wno-orphans #-}

module Autogestao.Driven.Report (Report (..), Listing (..)) where

import Autogestao.Core.Event qualified as E
import Autogestao.Core.Task
import Data.Foldable (maximum)
import Data.Text (strip)
import Data.Text qualified as T
import System.Console.Pretty

data Report
  = EventReport E.Event
  | ListingReport Listing
  | ErrorReport Text
  deriving stock (Show, Eq)

newtype Listing = TaskListing [Task]
  deriving stock (Show, Eq)

instance ToText Report where
  toText = \case
    EventReport event -> eventToText event
    ListingReport listing -> listingToText listing
    ErrorReport msg -> msg

eventToText :: E.Event -> Text
eventToText = \case
  E.TaskEvent event -> case event of
    Created task -> "Task " <> (show . rawId $ task) <> " created"
    Updated task -> "Task " <> (show . rawId $ task) <> " updated"
    NotFound taskId -> "Task " <> show taskId.unId <> " not found"

rawId :: Task -> Int
rawId = unId . taskId

data RawTask = Raw
  { pId :: Text
  , description :: Text
  , status :: Text
  , tags :: [Text]
  , originalStatus :: TaskStatus
  }
  deriving stock (Eq, Show)

toPrintable :: Task -> RawTask
toPrintable (Task (TaskId taskId) (Desc desc) status tags _) =
  Raw
    { pId = show taskId
    , description = desc
    , status = show status
    , tags = fmap unTag tags
    , originalStatus = status
    }

listingToText :: Listing -> Text
listingToText = \case
  TaskListing tasks -> case tasks of
    [] -> "no task found"
    _ -> printGrid tasks

printGrid :: [Task] -> Text
printGrid tasks =
  let maxLenId = (+ 1) . maximum . fmap T.length . ("Id" :) . fmap (show . unId . taskId) $ tasks
      headers =
        headerStyle . fmt maxLenId $
          Raw
            { pId = "Id"
            , description = "Description"
            , status = "Status"
            , tags = []
            , originalStatus = Todo
            }
      formattedRows = tasks <&> colors . fmt maxLenId . toPrintable
      rows = (headers : formattedRows) <&> \(Raw pId desc status tags _) -> strip $ status <> pId <> desc <> " " <> unwords tags
   in unlines rows

padLeft :: Int -> Text -> Text
padLeft maxLen txt' = go (maxLen - T.length txt') txt'
  where
    go len txt | len <= 0 = txt
    go n txt = go (n - 1) $ txt <> " "

headerStyle :: RawTask -> RawTask
headerStyle r@(Raw pId desc status _ _) =
  r
    { pId = style Faint pId
    , description = style Faint desc
    , status = style Faint status
    }
fmt :: Int -> RawTask -> RawTask
fmt maxLenId (Raw pId desc status tags orig) =
  Raw
    { pId = padLeft maxLenId pId
    , description = desc
    , status = padLeft 10 status
    , tags = fmap ("#" <>) tags
    , originalStatus = orig
    }

colors :: RawTask -> RawTask
colors (Raw pId desc status tags orig) =
  Raw
    { pId = styleWith orig pId
    , description = styleWith orig desc
    , status = statusStyle orig status
    , tags = fmap (style Faint) tags
    , originalStatus = orig
    }

strikethrough :: Text -> Text
strikethrough txt = "\x1b[9m" <> txt <> "\x1b[0m"

styleWith :: TaskStatus -> Text -> Text
styleWith = \case
  Done -> style Faint . style Italic
  Canceled -> style Faint . style Italic . strikethrough
  _ -> identity

statusStyle :: TaskStatus -> Text -> Text
statusStyle orig status =
  status & case orig of
    Todo -> color Magenta . style Bold
    Next -> color Yellow . style Bold
    Ongoing -> color Green . style Bold
    Done -> color Blue . style Italic
    Canceled -> style Faint . style Italic . strikethrough
