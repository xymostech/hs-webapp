module Handler
( Handler
, runHandler
)
where

import Control.Monad.Reader   (ReaderT, runReaderT)
import Database.SQLite.Simple (Connection, open)
import Network.Wai            (Response)

data HandlerData = HandlerData
  { dbConnection :: Connection
  }

type Handler = ReaderT HandlerData IO Response

runHandler :: Handler -> IO Response
runHandler handler = do
  connection <- open "datastore.sqlite"
  runReaderT handler
             HandlerData { dbConnection = connection
                         }
