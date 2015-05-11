module Handler
( Handler
, runHandler
, dbConnection
, ask
, asks
)
where

import Control.Monad.Reader   (ReaderT, runReaderT, ask, asks)
import Database.SQLite.Simple (Connection, open, close)
import Network.Wai            (Response)

data HandlerData = HandlerData
  { dbConnection :: Connection
  }

type Handler a = ReaderT HandlerData IO a

runHandler :: Handler Response -> IO Response
runHandler handler = do
  connection <- open "datastore.sqlite"
  resp <- runReaderT handler
                     HandlerData { dbConnection = connection
                                 }
  close connection
  return resp
