{-# LANGUAGE OverloadedStrings,
             DataKinds,
             TypeFamilies,
             FlexibleContexts,
             GADTs,
             ScopedTypeVariables #-}
module DB.DB
( dbSetup
, TestData(TestData)
, testId, testName, testKey, testRef
, query, query'
, QueryComparator((:=:), (:/=:), (:<:), (:<=:), (:>:), (:>=:))
, DBKey(DBKey, dbKey)
, DBInt(DBInt, dbInt)
, DBText(DBText, dbText)
, DBForeignKey(DBForeignKey, dbForeignKey)
)
where

import Control.Applicative    ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Text as T         (Text, intercalate, concat, append)
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

data TestData = TestData
  { testKey :: DBKey TestData
  , testId :: DBInt "id"
  , testName :: DBText "name"
  , testDate :: DBDate "added"
  , testRef :: Maybe (DBForeignKey TestData "friend")
  }
  deriving Show

instance DBType TestData where
  key = testKey
  name _ = "TestData"
  fields _ = [ mkField testKey
             , mkField testId
             , mkField testName
             , mkField testDate
             , mkField testRef
             ]

instance FromRow TestData where
  fromRow = TestData <$> field <*> field <*> field <*> field <*> field

data QueryComparator a where
  (:=:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a
  (:/=:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a
  (:<:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a
  (:<=:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a
  (:>:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a
  (:>=:) :: (DBFieldType (a -> f), ToField v) => (a -> f) -> v -> QueryComparator a

paramFromComparator :: QueryComparator a -> NamedParam
paramFromComparator (k :=: v) = (append "@" $ keyName k) := v
paramFromComparator (k :/=: v) = (append "@" $ keyName k) := v
paramFromComparator (k :<: v) = (append "@" $ keyName k) := v
paramFromComparator (k :<=: v) = (append "@" $ keyName k) := v
paramFromComparator (k :>: v) = (append "@" $ keyName k) := v
paramFromComparator (k :>=: v) = (append "@" $ keyName k) := v

selectorFromComparator :: QueryComparator a -> Text
selectorFromComparator (k :=: _) = T.concat [n, "=@", n] where n = keyName k
selectorFromComparator (k :/=: _) = T.concat [n, "!=@", n] where n = keyName k
selectorFromComparator (k :<: _) = T.concat [n, "<@", n] where n = keyName k
selectorFromComparator (k :<=: _) = T.concat [n, "<=@", n] where n = keyName k
selectorFromComparator (k :>: _) = T.concat [n, ">@", n] where n = keyName k
selectorFromComparator (k :>=: _) = T.concat [n, ">=@", n] where n = keyName k

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
    query = makeQuery $ T.concat
      [ "SELECT * FROM "
      , name (undefined :: a)
      , if null comparators then "" else " WHERE "
      , intercalate " AND " $ map selectorFromComparator comparators
      ]

setupTable :: forall a. DBType a => Connection -> IO a
setupTable conn = do
  execute_ conn query
  return undefined
  where
    fieldDefs = map (\(MkField f) -> createStatement f) $
                    fields (undefined :: a)
    fieldConstraints = Prelude.concat $ map (\(MkField f) -> constraints f) $
                                            fields (undefined :: a)
    query = makeQuery $ T.concat
      [ "CREATE TABLE IF NOT EXISTS "
      , name (undefined :: a)
      , "("
      , intercalate ", " $ fieldDefs ++ fieldConstraints
      , ")"
      ]

dbSetup :: IO ()
dbSetup = do
  conn <- open "datastore.sqlite"
  setupTable conn :: IO TestData
  close conn
