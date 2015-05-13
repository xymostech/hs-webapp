{-# LANGUAGE OverloadedStrings,
             DataKinds #-}
module TestData
( TestData(testKey, testId, testName, testDate, testRef)
, makeTestData
)
where

import Data.Text       (Text)
import Data.Time.Clock (getCurrentTime)

import DB.DB

data TestData = TestData
  { testKey :: Maybe (DBKey TestData)
  , testId :: DBInt "id"
  , testName :: DBText "name"
  , testDate :: DBDate "added"
  , testRef :: Maybe (DBForeignKey TestData "friend")
  }
  deriving Show

makeTestData :: Int -> Text -> Maybe (DBKey TestData) -> IO TestData
makeTestData key name friend = do
  time <- getCurrentTime
  return $ TestData
    { testKey = Nothing
    , testId = DBInt key
    , testName = DBText name
    , testDate = DBDate time
    , testRef = case friend of
        Nothing -> Nothing
        Just k -> Just $ DBForeignKey k
    }

instance DBType TestData where
  key = testKey
  name _ = "TestData"
  fields _ = [ mkField testKey
             , mkField testId
             , mkField testName
             , mkField testDate
             , mkField testRef
             ]

instance FromRow TestData where
  fromRow = do
    key <- field
    id <- field
    name <- field
    date <- field
    ref <- field
    return $ TestData { testKey = key
                      , testId = id
                      , testName = name
                      , testDate = date
                      , testRef = ref
                      }
