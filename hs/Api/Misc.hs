{-# LANGUAGE OverloadedStrings #-}
module Api.Misc
( ping
)
where

import Network.HTTP.Types (status200)
import Network.Wai        (Response, Request, responseLBS)

import Handler            (Handler)

ping :: Request -> Handler Response
ping _ =
  return $ responseLBS status200 [] "pong"

