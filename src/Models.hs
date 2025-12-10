{-# LANGUAGE OverloadedStrings #-}
module Models where

import Data.Text (Text)
import Data.Time (Day, UTCTime)

data ResourceType = ResourceType
  { rtId   :: Int
  , rtName :: Text
  } deriving (Show, Eq)

data Resource = Resource
  { rId            :: Int
  , rTitle         :: Text
  , rTypeId        :: Int
  , rAbstract      :: Maybe Text
  , rPurpose       :: Maybe Text
  , rOpenedAt      :: Maybe Day
  , rUsageTermDays :: Maybe Int
  , rUsageTerms    :: Maybe Text
  } deriving (Show, Eq)

data Author = Author
  { aId       :: Int
  , aFullName :: Text
  , aDept     :: Maybe Text
  } deriving (Show, Eq)

data User = User
  { uId       :: Int
  , uUsername :: Text
  , uEmail    :: Maybe Text
  , uRole     :: Text
  } deriving (Show, Eq)

data Url = Url
  { urlId         :: Int
  , urlResourceId :: Int
  , urlUrl        :: Text
  , urlKind       :: Maybe Text
  } deriving (Show, Eq)

data UsageEvent = UsageEvent
  { ueId         :: Integer
  , ueResourceId :: Int
  , ueUserId     :: Maybe Int
  , ueEventAt    :: UTCTime
  , ueAction     :: Text
  } deriving (Show, Eq)

data ResourceStats = ResourceStats
  { rsResourceId :: Int
  , rsTitle      :: Text
  , rsViews      :: Int
  , rsDownloads  :: Int
  , rsLikes      :: Int
  , rsBookmarks  :: Int
  } deriving (Show, Eq)
