{-# LANGUAGE OverloadedStrings #-}
module Api.Misc
( pingHandler
)
where

import Network.HTTP.Types (status200)
import Network.Wai        (Response, Request, responseLBS)

import Handler            (Handler)

pingHandler :: Request -> Handler Response
pingHandler _ =
  return $ responseLBS status200 [] "pong"
