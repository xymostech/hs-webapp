{-# LANGUAGE OverloadedStrings #-}
module Api.Counter
( dbTest
)
where

import Control.Monad.IO.Class  (liftIO)
import Data.ByteString.Builder (intDec)
import Network.HTTP.Types      (status200)
import Network.Wai             ( Response, Request
                               , responseLBS, responseBuilder
                               )

import DB.DB
import TestData
import Util                    (textToLBS, bsToString, queryParameter)
import Handler                 (Handler)

dbTest :: Request -> Handler Response
dbTest req = do
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
