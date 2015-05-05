{-# LANGUAGE OverloadedStrings #-}
module Util
( plainFileResponse
, notFoundResponse
, serverErrorResponse
)
where

import Prelude hiding (readFile)
import Network.Wai              (Response, Request, responseFile)
import Network.HTTP.Types       (status200, status404, status500)
import System.Directory         (doesFileExist)
import Data.ByteString          (ByteString)
import Data.ByteString.Lazy     (readFile)

plainFileResponse :: FilePath -> ByteString -> IO Response
plainFileResponse path mimeType = do
  fileExists <- doesFileExist path
  if fileExists
  then return $ responseFile status200 [("Content-Type", mimeType)] path Nothing
  else notFoundResponse

notFoundResponse :: IO Response
notFoundResponse = do
  return $ responseFile status404 [("Content-Type", "text/html")] "static/404.html" Nothing

serverErrorResponse :: IO Response
serverErrorResponse = do
  return $ responseFile status500 [("Content-Type", "text/html")] "static/500.html" Nothing
