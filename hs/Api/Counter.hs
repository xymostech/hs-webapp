{-# LANGUAGE OverloadedStrings #-}
module Api.Counter
( getCounter
)
where

import Control.Monad.IO.Class  (liftIO)
import Data.ByteString.Builder (intDec)
import Data.Text               (Text, unpack)
import Network.HTTP.Types      (status200)
import Network.Wai             ( Response, Request
                               , responseLBS, responseBuilder
                               )

import DB.DB
import TestData
import Util                    (textToLBS, bsToString, queryParameter)
import Handler                 (Handler)

idFromCount :: Text -> Int
idFromCount count = read $ unpack count

counterFromId :: Text -> Handler (Maybe TestData)
counterFromId count = do
  counters <- query [testId :=: (DBInt $ idFromCount count)]
  return $ case counters of
    [] -> Nothing
    (counter:_) -> Just counter

getCounter :: Request -> Text -> Handler Response
getCounter req count = do
  maybeCounter <- counterFromId count
  newCount <- case maybeCounter of
    Just counter -> do
      put $ counter { testCount = DBInt $ 1 + (dbInt $ testCount counter) }
    _ -> do
      put $ makeTestData $ idFromCount count
  return $ responseBuilder status200 [] (intDec $ dbInt $ testCount newCount)
