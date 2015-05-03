{-# LANGUAGE OverloadedStrings #-}
module Util
( plainFileResponse
, notFoundResponse
, serverErrorResponse
)
where

import Prelude hiding (readFile)
import Network.Wai              (Response, Request, responseLBS)
import Network.HTTP.Types       (status200, status404, status500)
import System.Directory         (doesFileExist)
import Data.ByteString          (ByteString)
import Data.ByteString.Lazy     (readFile)

plainFileResponse :: FilePath -> ByteString -> IO Response
plainFileResponse path mimeType = do
  fileExists <- doesFileExist path
  if fileExists
  then do
    fileData <- readFile path
    return $ responseLBS status200 [("Content-Type", mimeType)] fileData
  else notFoundResponse

notFoundResponse :: IO Response
notFoundResponse = do
  fileData <- readFile "static/404.html"
  return $ responseLBS status404 [("Content-Type", "text/html")] fileData

serverErrorResponse :: IO Response
serverErrorResponse = do
  fileData <- readFile "static/500.html"
  return $ responseLBS status500 [("Content-Type", "text/html")] fileData
