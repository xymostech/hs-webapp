{-# LANGUAGE OverloadedStrings #-}
module Api.Api
( apiHandler
)
where

import Network.HTTP.Types (status404)
import Network.Wai        (Response, Request, pathInfo, responseLBS)

import Handler            (Handler)

import Api.Misc           (pingHandler)

unknownApiHandler :: Handler
unknownApiHandler =
  return $ responseLBS status404 [] "Invalid API Request"

apiHandler :: Request -> Handler
apiHandler req = case path of
  ["api", "v1", "ping"] -> pingHandler req
  _ -> unknownApiHandler
  where
    path = pathInfo req
