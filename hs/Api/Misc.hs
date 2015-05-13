{-# LANGUAGE OverloadedStrings #-}
module Api.Misc
( pingHandler
, dbTestHandler
)
where

import Data.ByteString.Builder (intDec)
import Data.List               (find)
import Network.HTTP.Types      (status200)
import Network.Wai             (Response, Request, responseLBS, responseBuilder)

import DB.DB
import TestData
import Handler                 (Handler)
import Util                    (textToLBS, bsToString, queryParameter)

pingHandler :: Request -> Handler Response
pingHandler _ =
  return $ responseLBS status200 [] "pong"

dbTestHandler :: Request -> Handler Response
dbTestHandler req = do
  counts <- query [testId :=: DBInt countId]
  newCount <- case counts of
    (count:_) -> do
      put $ count { testCount = DBInt $ 1 + (dbInt $ testCount count) }
    _ -> do
      put $ makeTestData countId
  return $ responseBuilder status200 [] (intDec $ dbInt $ testCount newCount)
  where
    Just countIdBS = queryParameter req "count"
    countId = read $ bsToString countIdBS
