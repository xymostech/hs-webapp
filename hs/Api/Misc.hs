{-# LANGUAGE OverloadedStrings #-}
module Api.Misc
( pingHandler
, dbTestHandler
)
where

import Data.ByteString.Lazy
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status200)
import Network.Wai        (Response, Request, responseLBS)

import DB.DB
import TestData
import Handler            (Handler)
import Util               (textToLBS)

pingHandler :: Request -> Handler Response
pingHandler _ =
  return $ responseLBS status200 [] "pong"

dbTestHandler :: Request -> Handler Response
dbTestHandler _ = do
  (a:_) <- query [testId :=: DBInt 3]
  return $ responseLBS status200 [] $ textToLBS $ dbText $ testName a
