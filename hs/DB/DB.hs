{-# LANGUAGE OverloadedStrings,
             DataKinds,
             ExistentialQuantification,
             TypeFamilies,
             FlexibleContexts,
             GADTs,
             ScopedTypeVariables #-}
module DB.DB
( dbSetup
, TestData(TestData)
, testId, testName
, query
, QueryComparator((:=:))
, DBInt(DBInt, dbInt), DBText(DBText, dbText)
)
where

import Prelude                hiding (concat)
import Control.Applicative    ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text, intercalate, concat, append)
import Data.String            (fromString)
import Database.SQLite.Simple ( FromRow(fromRow)
                              , NamedParam
                              , Query(Query, fromQuery)
                              , Connection, open, close
                              , execute_
                              , queryNamed
                              , field
                              , NamedParam((:=))
                              )
import Database.SQLite.Simple.ToField

import Handler

import DB.Types

data DBField a = forall f . DBFieldType f => MkField f
class FromRow a => DBType a where
  mkField :: DBFieldType (a -> f) => (a -> f) -> DBField a
  mkField = MkField

  dbName :: a -> Text
  dbFields :: a -> [DBField a]

data TestData = TestData
  { testId :: DBInt "id"
  , testName :: DBText "name"
  }
  deriving Show

instance DBType TestData where
  dbName _ = "TestData"
  dbFields _ = [mkField testId, mkField testName]

instance FromRow TestData where
  fromRow = TestData <$> field <*> field

type TableName = Text

data QueryComparator a where
  (:=:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a

paramFromComparator :: QueryComparator a -> NamedParam
paramFromComparator (k :=: v) = (append "@" $ keyName k) := v

selectorFromComparator :: QueryComparator a -> Text
selectorFromComparator (k :=: _) = concat [n, "=@", n]
  where
    n = keyName k

makeQuery :: Text -> Query
makeQuery text = Query { fromQuery = text }

query :: DBType a => [QueryComparator a] -> Handler [a]
query comparators = do
  conn <- asks dbConnection
  liftIO $ query' comparators conn

query' :: forall a. DBType a => [QueryComparator a] -> Connection -> IO [a]
query' comparators conn = do
  queryNamed conn query namedParams
  where
    namedParams = map paramFromComparator comparators
    query = makeQuery $ concat
      [ "SELECT * FROM "
      , dbName (undefined :: a)
      , " WHERE "
      , intercalate " AND " $ map selectorFromComparator comparators
      ]

setupTable :: forall a. DBType a => Connection -> IO a
setupTable conn = do
  execute_ conn query
  return undefined
  where
    name = dbName (undefined :: a)
    fields = dbFields (undefined :: a)
    query = makeQuery $ concat
      [ "CREATE TABLE IF NOT EXISTS "
      , name
      , "("
      , intercalate ", " $ map (\(MkField f) -> concat [keyName f, " ", typeName f])
                               fields
      , ")"
      ]

dbSetup :: IO ()
dbSetup = do
  conn <- open "datastore.sqlite"
  setupTable conn :: IO TestData
  close conn
