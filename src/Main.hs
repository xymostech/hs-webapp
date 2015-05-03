{-# LANGUAGE OverloadedStrings #-}
import Control.Exception.Lifted       (handle)
import Control.Exception              (SomeException)
import Data.ByteString.Lazy           (toStrict)
import qualified Data.ByteString as B (intercalate, concat)
import qualified Data.ByteString.Char8 as C (putStrLn)
import Network.HTTP.Types             (statusCode)
import Network.Wai                    ( Application, Response, Request
                                      , rawPathInfo, rawQueryString
                                      , pathInfo, requestMethod, responseStatus
                                      )
import Text.Show.ByteString           (show)
import Network.Wai.Handler.Warp       (run)

import Static                         (staticHandler)
import Util                           (notFoundResponse, serverErrorResponse)

main :: IO ()
main = run 3000 app

printStatusLine :: Request -> Response -> IO ()
printStatusLine req resp =
  C.putStrLn $ B.intercalate " "
    [ requestMethod req
    , rawPathInfo req
    , rawQueryString req
    , toStrict $ Text.Show.ByteString.show $ statusCode $ responseStatus resp
    ]

app :: Application
app req sendResponse = handle (\err -> handleError err req >>= sendResponse) $ do
  resp <- handler req
  printStatusLine req resp
  sendResponse resp
  where
    handler = chooseHandler req


chooseHandler :: Request -> (Request -> IO Response)
chooseHandler req = case path of
  "static":_ -> staticHandler
  _ -> \x -> notFoundResponse
  where
    path = pathInfo req

handleError :: SomeException -> Request -> IO Response
handleError ex req = do
  resp <- serverErrorResponse
  printStatusLine req resp
  return resp
