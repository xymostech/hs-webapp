{-# LANGUAGE OverloadedStrings #-}
module Static
( staticHandler
)
where

import Data.Text              (concat, unpack, pack)
import Network.Mime           (defaultMimeLookup)
import Network.Wai            (Application, Response, Request, pathInfo)
import System.FilePath        (joinPath)

import Handler                (Handler)
import Util                   (plainFileResponse)

staticHandler :: Request -> Handler Response
staticHandler req =
  plainFileResponse filePath mimeType
  where
    filePath = joinPath $ map unpack $ pathInfo req
    mimeType = defaultMimeLookup $ pack filePath
