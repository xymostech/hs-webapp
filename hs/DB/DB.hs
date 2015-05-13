{-# LANGUAGE OverloadedStrings,
             DataKinds,
             TypeFamilies,
             FlexibleContexts,
             GADTs,
             ScopedTypeVariables #-}
module DB.DB
( dbSetup
, query, query'
, put, put'
, QueryComparator((:=:), (:/=:), (:<:), (:<=:), (:>:), (:>=:))
, DBType(mkField, key, setKey, name, fields)
, DBInt(DBInt, dbInt)
, DBText(DBText, dbText)
, DBByteString(DBByteString, dbByteString)
, DBDouble(DBDouble, dbDouble)
, DBBool(DBBool, dbBool)
, DBDate(DBDate, dbDate)
, DBKey(DBKey, dbKey)
, DBForeignKey(DBForeignKey, dbForeignKey)
, FromRow(fromRow), field
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
                              , execute, executeNamed, execute_
                              , queryNamed
                              , lastInsertRowId
                              , field
                              , NamedParam((:=))
                              )
import Database.SQLite.Simple.ToField

import Handler

import DB.Types

data QueryComparator a where
  (:=:) :: DBFieldType a f => (a -> f) -> f -> QueryComparator a
  (:/=:) :: DBFieldType a f => (a -> f) -> f -> QueryComparator a
  (:<:) :: DBFieldType a f => (a -> f) -> f -> QueryComparator a
  (:<=:) :: DBFieldType a f => (a -> f) -> f -> QueryComparator a
  (:>:) :: DBFieldType a f => (a -> f) -> f -> QueryComparator a
  (:>=:) :: DBFieldType a f => (a -> f) -> f -> QueryComparator a

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

insert' :: forall a. DBType a => a -> Connection -> IO a
insert' insertData conn = do
  execute conn query (ToRowDBType insertData)
  rowId <- lastInsertRowId conn
  return $ setKey insertData $ Just (DBKey $ fromIntegral rowId)
  where
    query = makeQuery $ T.concat
      [ "INSERT INTO "
      , name (undefined :: a)
      , " VALUES ("
      , intercalate ", " $ map (\_ -> "?") $ fields (undefined :: a)
      , ")"
      ]

update' :: forall a. DBType a => a -> Connection -> IO ()
update' updateData conn = do
  executeNamed conn query $ namedParams updateData
  where
    query = makeQuery $ T.concat
      [ "UPDATE "
      , name (undefined :: a)
      , " SET "
      , intercalate ", " $ selectors (undefined :: a)
      , " WHERE rowid=@rowid"
      ]

put :: DBType a => a -> Handler a
put putData = do
  conn <- asks dbConnection
  liftIO $ put' putData conn

put' :: forall a. DBType a => a -> Connection -> IO a
put' putData conn = case key putData of
  Just k -> do
    update' putData conn
    return putData
  Nothing -> insert' putData conn

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
