{-# LANGUAGE OverloadedStrings #-}
module Api.Counter
( getCounter
, putCounter
)
where

import Control.Monad.IO.Class  (liftIO)
import Data.ByteString.Builder (intDec)
import Data.ByteString.Lazy.Char8 as LB8
import Data.Text as T          (Text, unpack)
import Network.HTTP.Types      (status200, status404)
import Network.Wai             ( Response, Request
                               , responseLBS, responseBuilder
                               , lazyRequestBody
                               )

import DB.DB
import TestData
import Util                    (textToLBS, bsToString, queryParameter)
import Handler                 (Handler)

idFromCount :: Text -> Int
idFromCount count = read $ T.unpack count

counterFromId :: Text -> Handler (Maybe TestData)
counterFromId count = do
  counters <- query [testId :=: (DBInt $ idFromCount count)]
  return $ case counters of
    [] -> Nothing
    (counter:_) -> Just counter

getCounter :: Request -> Text -> Handler Response
getCounter req count = do
  maybeCounter <- counterFromId count
  return $ case maybeCounter of
    Nothing -> responseLBS status404 [] "Unknown counter"
    Just counter -> responseBuilder status200 [] (intDec $ dbInt $ testCount counter)

putCounter :: Request -> Text -> Handler Response
putCounter req count = do
  body <- liftIO $ lazyRequestBody req
  let value = read $ LB8.unpack body
  maybeCounter <- counterFromId count
  let counter = case maybeCounter of
                  Nothing -> makeTestData $ idFromCount count
                  Just counter -> counter
  put $ counter { testCount = DBInt value }
  return $ responseLBS status200 [] ""
