{-# LANGUAGE OverloadedStrings #-}
module Api.Misc
( pingHandler
)
where

import Network.HTTP.Types (status200)
import Network.Wai        (Response, Request, responseLBS)

pingHandler :: Request -> IO Response
pingHandler _ =
  return $ responseLBS status200 [] "pong"
