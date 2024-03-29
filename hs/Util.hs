{-# LANGUAGE OverloadedStrings #-}
module Util
( plainFileResponse
, notFoundResponse
, serverErrorResponse
, textToLBS, bsToString
, queryParameter
)
where

import Control.Monad.IO.Class   (liftIO)
import Data.List                (find)
import Data.Text                (Text)
import Data.Text.Encoding       (encodeUtf8)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types       (status200, status404, status500)
import Network.Wai              (Response, Request, responseFile, queryString)
import System.Directory         (doesFileExist)

import Handler                  (Handler)

plainFileResponse :: FilePath -> BS.ByteString -> Handler Response
plainFileResponse path mimeType = do
  fileExists <- liftIO $ doesFileExist path
  if fileExists
  then return $ responseFile status200 [("Content-Type", mimeType)] path Nothing
  else notFoundResponse

notFoundResponse :: Handler Response
notFoundResponse = do
  return $ responseFile status404 [("Content-Type", "text/html")] "static/404.html" Nothing

serverErrorResponse :: Handler Response
serverErrorResponse = do
  return $ responseFile status500 [("Content-Type", "text/html")] "static/500.html" Nothing

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . encodeUtf8

bsToString :: BS.ByteString -> String
bsToString = BS8.unpack

queryParameter :: Request -> BS.ByteString -> Maybe BS.ByteString
queryParameter req param = do
  (_, value) <- find (\(p, _) -> p == param) $ queryString req
  value
