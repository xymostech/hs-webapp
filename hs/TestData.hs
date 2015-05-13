{-# LANGUAGE OverloadedStrings,
             DataKinds #-}
module TestData
( TestData(testKey, testId, testCount)
, makeTestData
)
where

import Data.Text       (Text)
import Data.Time.Clock (getCurrentTime)

import DB.DB

data TestData = TestData
  { testKey :: Maybe (DBKey TestData)
  , testId :: DBInt "id"
  , testCount :: DBInt "count"
  }
  deriving Show

makeTestData :: Int -> TestData
makeTestData id = TestData Nothing (DBInt id) (DBInt 0)

instance DBType TestData where
  key = testKey
  setKey testData newKey = testData { testKey = newKey }
  name _ = "TestData"
  fields _ = [ mkField testKey
             , mkField testId
             , mkField testCount
             ]

instance FromRow TestData where
  fromRow = do
    key <- field
    id <- field
    count <- field
    return $ TestData { testKey = key
                      , testId = id
                      , testCount = count
                      }
