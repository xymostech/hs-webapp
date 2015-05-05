{-# LANGUAGE OverloadedStrings #-}
module Api.Api
( apiHandler
)
where

import Network.HTTP.Types (status404)
import Network.Wai        (Response, Request, pathInfo, responseLBS)

import Api.Misc           (pingHandler)

unknownApiHandler :: IO Response
unknownApiHandler =
  return $ responseLBS status404 [] "Invalid API Request"

apiHandler :: Request -> IO Response
apiHandler req = case path of
  ["api", "v1", "ping"] -> pingHandler req
  _ -> unknownApiHandler
  where
    path = pathInfo req
