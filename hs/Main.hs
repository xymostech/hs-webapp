{-# LANGUAGE OverloadedStrings #-}
import Control.Exception.Lifted       (handle)
import Control.Exception              (SomeException, displayException)
import Control.Monad.IO.Class         (liftIO)
import Data.ByteString.Lazy           (toStrict)
import qualified Data.ByteString as B (intercalate, concat)
import qualified Data.ByteString.Char8 as C (putStrLn)
import Network.HTTP.Types             (statusCode)
import Network.HTTP.Types.Method      (methodGet)
import Network.Wai                    ( Application, Response, Request
                                      , rawPathInfo, rawQueryString
                                      , pathInfo, requestMethod, responseStatus
                                      )
import System.Environment             (getArgs)
import Text.Show.ByteString           (show)
import Network.Wai.Handler.Warp       (run)

import Api.Api                        (apiHandler)
import DB.DB                          (dbSetup)
import Handler                        (Handler, runHandler)
import Static                         (staticHandler)
import Util                           ( plainFileResponse
                                      , notFoundResponse
                                      , serverErrorResponse
                                      )
import TestData                       (TestData)
import qualified Logging

main :: IO ()
main = do
  dbSetup $ \setupTable -> do
    setupTable :: IO TestData
    return ()
  run 7000 app

printStatusLine :: Request -> Response -> IO ()
printStatusLine req resp =
  C.putStrLn $ B.intercalate " "
    [ requestMethod req
    , rawPathInfo req
    , rawQueryString req
    , toStrict $ Text.Show.ByteString.show $ statusCode $ responseStatus resp
    ]

app :: Application
app req sendResponse =
  handle (\err -> runHandler (handleError err req) >>= sendResponse) $ do
    -- TODO(emily): move this into main
    debug <- isDebug
    resp <- runHandler $ (chooseHandler req debug) req
    printStatusLine req resp
    sendResponse resp

chooseHandler :: Request -> Bool -> (Request -> Handler Response)
chooseHandler req debug = case (method, path, debug) of
  (methodGet, "static":_, _) -> staticHandler
  (methodGet, "build":_, _) -> staticHandler
  (_, "api":_, _) -> apiHandler
  (methodGet, _, False) -> \x -> plainFileResponse "static/index.html" "text/html"
  (methodGet, _, True) -> \x -> plainFileResponse "static/index-debug.html" "text/html"
  where
    method = requestMethod req
    path = pathInfo req

handleError :: SomeException -> Request -> Handler Response
handleError ex req = do
  liftIO $ print $ displayException ex
  resp <- serverErrorResponse
  liftIO $ printStatusLine req resp
  return resp

isDebug :: IO Bool
isDebug = getArgs >>= return . any (=="--debug")
