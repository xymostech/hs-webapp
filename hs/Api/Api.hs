{-# LANGUAGE OverloadedStrings #-}
module Api.Api
( apiHandler
)
where

import Network.HTTP.Types        (status404)
import Network.HTTP.Types.Method (methodGet)
import Network.Wai               ( Response, Request
                                 , requestMethod, pathInfo
                                 , responseLBS
                                 )

import Handler                   (Handler)

import Api.Misc                  (pingHandler, dbTestHandler)

unknownApiHandler :: Handler Response
unknownApiHandler =
  return $ responseLBS status404 [] "Invalid API Request"

apiHandler :: Request -> Handler Response
apiHandler req = case (method, path) of
  (methodGet, ["api", "v1", "ping"]) -> pingHandler req
  (methodGet, ["api", "v1", "counter"]) -> dbTestHandler req
  _ -> unknownApiHandler
  where
    method = requestMethod req
    path = pathInfo req
