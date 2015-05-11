{-# LANGUAGE OverloadedStrings #-}
module Api.Api
( apiHandler
)
where

import Network.HTTP.Types (status404)
import Network.Wai        (Response, Request, pathInfo, responseLBS)

import Handler            (Handler)

import Api.Misc           (pingHandler, dbTestHandler)

unknownApiHandler :: Handler Response
unknownApiHandler =
  return $ responseLBS status404 [] "Invalid API Request"

apiHandler :: Request -> Handler Response
apiHandler req = case path of
  ["api", "v1", "ping"] -> pingHandler req
  ["api", "v1", "dbtest"] -> dbTestHandler req
  _ -> unknownApiHandler
  where
    path = pathInfo req
