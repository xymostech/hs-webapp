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
import System.Environment             (getArgs)
import Text.Show.ByteString           (show)
import Network.Wai.Handler.Warp       (run)

import Static                         (staticHandler)
import Util                           ( plainFileResponse
                                      , notFoundResponse
                                      , serverErrorResponse
                                      )
import qualified Logging

main :: IO ()
main = run 7000 app

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
  -- TODO(emily): move this into main
  debug <- isDebug
  resp <- (chooseHandler req debug) req
  printStatusLine req resp
  sendResponse resp

chooseHandler :: Request -> Bool -> (Request -> IO Response)
chooseHandler req debug = case (path, debug) of
  ([], False) -> \x -> plainFileResponse "static/index.html" "text/html"
  ([], True) -> \x -> plainFileResponse "static/index-debug.html" "text/html"
  ("static":_, _) -> staticHandler
  ("build":_, _) -> staticHandler
  _ -> \x -> notFoundResponse
  where
    path = pathInfo req

handleError :: SomeException -> Request -> IO Response
handleError ex req = do
  resp <- serverErrorResponse
  printStatusLine req resp
  return resp

isDebug :: IO Bool
isDebug = getArgs >>= return . any (=="--debug")
