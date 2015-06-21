{-# LANGUAGE GADTs,
             OverloadedStrings,
             ScopedTypeVariables #-}
module DB.DB
( dbSetup
, query, query'
, put, put'
, delete, delete'
, QueryComparator((:=:), (:/=:), (:<:), (:<=:), (:>:), (:>=:))
, DBType(mkField, key, setKey, name, fields)
, DBInt(DBInt, dbInt)
, DBText(DBText, dbText)
, DBByteString(DBByteString, dbByteString)
, DBDouble(DBDouble, dbDouble)
, DBBool(DBBool, dbBool)
, DBDate(DBDate, dbDate)
, DBKey(dbKey)
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
                              , queryNamed, query_
                              , lastInsertRowId
                              , withTransaction
                              , field
                              , NamedParam((:=))
                              )
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Types ( Null
                                    )

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

delete :: DBType a => a -> Handler a
delete deleteData = do
  conn <- asks dbConnection
  liftIO $ delete' deleteData conn

delete' :: forall a. DBType a => a -> Connection -> IO a
delete' deleteData conn = do
  executeNamed conn query params
  return $ setKey deleteData Nothing
  where
    query = makeQuery $ T.concat
      [ "DELETE FROM "
      , name (undefined :: a)
      , " WHERE rowid=@rowid"
      ]
    params = ["@rowid" := key deleteData]

type TableInfoRow = (Int, T.Text, T.Text, Int, Null, Int)

tableInfo :: forall a. DBType a => Connection -> a -> IO [TableInfoRow]
tableInfo conn _ =
  query_ conn query
  where
    query = makeQuery $ T.concat
      [ "PRAGMA table_info('"
      , name (undefined :: a)
      , "')"
      ]

createTable :: forall a. DBType a => Connection -> Text -> IO a
createTable conn tableName = do
  execute_ conn query
  return undefined
  where
    fieldDefs = map (\(MkField f) -> createStatement f) $
                    fields (undefined :: a)
    fieldConstraints = Prelude.concat $ map (\(MkField f) -> constraints f) $
                                            fields (undefined :: a)
    query = makeQuery $ T.concat
      [ "CREATE TABLE IF NOT EXISTS "
      , tableName
      , "("
      , intercalate ", " $ fieldDefs ++ fieldConstraints
      , ")"
      ]

getMigrateFields :: forall a. DBType a => Connection -> a -> IO [Text]
getMigrateFields conn table = do
  info <- tableInfo conn table
  return $ migrateFields info
  where
    migrateFields info = map (fieldNameOrDefault info) $ fields table

    fieldNameOrDefault info field@(MkField f) =
      if fieldExistsInTable info field
      then keyName f
      else defaultValue f

    fieldExistsInTable info field = any (fieldMatchesTableInfo field) info

    fieldMatchesTableInfo (MkField f) (_, rowName, rowType, _, _, _) =
      (keyName f == rowName) && (typeName f == rowType)

migrate :: forall a. DBType a => Connection -> IO a
migrate conn = do
  migrateFields <- getMigrateFields conn (undefined :: a)
  withTransaction conn $ do
    (createTable conn migrateName) :: IO a
    execute_ conn $ copyQuery migrateFields
    execute_ conn dropQuery
    execute_ conn renameQuery
  return undefined
    where
      migrateName = T.concat [name (undefined :: a), "_migrate"]
      copyQuery migrateFields = makeQuery $ T.concat
        [ "INSERT INTO "
        , migrateName
        , " SELECT "
        , T.intercalate ", " migrateFields
        , " FROM "
        , name (undefined :: a)
        ]
      dropQuery = makeQuery $ T.concat
        [ "DROP TABLE "
        , name (undefined :: a)
        ]
      renameQuery = makeQuery $ T.concat
        [ "ALTER TABLE "
        , migrateName
        , " RENAME TO "
        , name (undefined :: a)
        ]

checkTableExists :: forall a. DBType a => Connection -> a -> IO Bool
checkTableExists conn table = do
  results <- (queryNamed conn query params) :: IO [[Text]]
  return $ length results > 0
  where
    query = makeQuery $ T.concat
      [ "SELECT name"
      , " FROM sqlite_master"
      , " WHERE type='table'"
      , " AND name=@tableName"
      ]
    params = ["@tableName" := name (undefined :: a)]

setupTable :: forall a. DBType a => Connection -> IO a
setupTable conn = do
  tableExists <- checkTableExists conn (undefined :: a)
  if tableExists
  then migrate conn
  else createTable conn $ name (undefined :: a)

dbSetup :: DBType a => (IO a -> IO ()) -> IO ()
dbSetup func = do
  conn <- open "datastore.sqlite"
  func (setupTable conn)
  close conn
