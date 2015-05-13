{-# LANGUAGE OverloadedStrings,
             DataKinds,
             TypeFamilies,
             FlexibleContexts,
             GADTs,
             ScopedTypeVariables #-}
module DB.DB
( dbSetup
, query, query'
, QueryComparator((:=:), (:/=:), (:<:), (:<=:), (:>:), (:>=:))
, DBType(mkField, key, name, fields)
, DBInt(DBInt, dbInt)
, DBText(DBText, dbText)
, DBByteString(DBByteString, dbByteString)
, DBDouble(DBDouble, dbDouble)
, DBBool(DBBool, dbBool)
, DBDate(DBDate, dbDate)
, DBKey(DBKey, dbKey)
, DBForeignKey(DBForeignKey, dbForeignKey)
, FromRow(fromRow), field
, ToRow(toRow), toField
)
where

import Control.Applicative    ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Text as T         (Text, intercalate, concat, append)
import Data.String            (fromString)
import Database.SQLite.Simple ( FromRow(fromRow)
                              , ToRow(toRow)
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

data QueryComparator a where
  (:=:) :: (DBFieldType a f, ToField v) => (a -> f) -> v -> QueryComparator a
  (:/=:) :: (DBFieldType a f, ToField v) => (a -> f) -> v -> QueryComparator a
  (:<:) :: (DBFieldType a f, ToField v) => (a -> f) -> v -> QueryComparator a
  (:<=:) :: (DBFieldType a f, ToField v) => (a -> f) -> v -> QueryComparator a
  (:>:) :: (DBFieldType a f, ToField v) => (a -> f) -> v -> QueryComparator a
  (:>=:) :: (DBFieldType a f, ToField v) => (a -> f) -> v -> QueryComparator a

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

dbSetup :: DBType a => (IO a -> IO ()) -> IO ()
dbSetup func = do
  conn <- open "datastore.sqlite"
  func (setupTable conn)
  close conn
