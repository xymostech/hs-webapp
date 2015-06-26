{-# LANGUAGE OverloadedStrings #-}
module Api.Api
( apiHandler
)
where

import Network.HTTP.Types        (status404)
import Network.Wai               ( Response, Request
                                 , requestMethod, pathInfo
                                 , responseLBS
                                 )

import Handler                   (Handler)

import qualified Api.Misc as Misc
import qualified Api.Counter as Counter

unknownApiHandler :: Handler Response
unknownApiHandler =
  return $ responseLBS status404 [] "Invalid API Request"

apiHandler :: Request -> Handler Response
apiHandler req = case (method, path) of
  ("GET", ["api", "v1", "ping"]) -> Misc.ping req
  ("GET", ["api", "v1", "counter", count]) -> Counter.getCounter req count
  ("PUT", ["api", "v1", "counter", count]) -> Counter.putCounter req count
  _ -> unknownApiHandler
  where
    method = requestMethod req
    path = pathInfo req
