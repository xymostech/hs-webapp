{-# LANGUAGE OverloadedStrings,
             DataKinds,
             KindSignatures,
             ExistentialQuantification,
             FlexibleContexts,
             DeriveDataTypeable,
             ScopedTypeVariables,
             FlexibleInstances #-}
module DB.Types
( DBKey(DBKey, dbKey)
, DBInt(DBInt, dbInt)
, DBText(DBText, dbText)
, DBForeignKey(DBForeignKey, dbForeignKey)
, DBFieldType
, keyName, typeName, createStatement, constraints
, DBField(MkField)
, DBType, mkField, key, name, fields
)
where

import Data.Int
import Data.Text
import Data.Typeable
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import GHC.TypeLits

class DBFieldType a where
  keyName :: a -> Text
  typeName :: a -> Text

  createStatement :: a -> Text
  createStatement x = Data.Text.concat [keyName x, " ", typeName x]

  constraints :: a -> [Text]
  constraints _ = []

data DBField a = forall f . DBFieldType f => MkField f
class FromRow a => DBType a where
  mkField :: DBFieldType (a -> f) => (a -> f) -> DBField a
  mkField = MkField

  key :: a -> DBKey
  name :: a -> Text
  fields :: a -> [DBField a]

newtype DBKey = DBKey { dbKey :: Int }
  deriving Show
newtype DBInt (key :: Symbol) = DBInt { dbInt :: Int }
  deriving Show
newtype DBText (key :: Symbol) = DBText { dbText :: Text }
  deriving Show
newtype DBType a => DBForeignKey a (key :: Symbol) = DBForeignKey { dbForeignKey :: Int }
  deriving Show

instance FromField DBKey where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBKey x)
    Errors e -> Errors e
instance Typeable k => FromField (DBInt k) where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBInt x)
    Errors e -> Errors e
instance Typeable k => FromField (DBText k) where
  fromField field = case (fromField field) :: Ok Text of
    Ok t     -> Ok (DBText t)
    Errors e -> Errors e
instance Typeable k => FromField (DBForeignKey k a) where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBForeignKey x)
    Errors e -> Errors e

instance DBFieldType (a -> DBKey) where
  keyName _ = "rowid"
  typeName _ = "INTEGER"
  createStatement x = Data.Text.concat [keyName x, " ", typeName x, " PRIMARY KEY"]
instance KnownSymbol k => DBFieldType (a -> DBInt k) where
  keyName _ = pack $ symbolVal (undefined :: DBInt k)
  typeName _ = "INTEGER"
instance KnownSymbol k => DBFieldType (a -> Maybe (DBInt k)) where
  keyName _ = pack $ symbolVal (undefined :: DBInt k)
  typeName _ = "INTEGER"
instance KnownSymbol k => DBFieldType (a -> DBText k) where
  keyName _ = pack $ symbolVal (undefined :: DBText k)
  typeName _ = "TEXT"
instance KnownSymbol k => DBFieldType (a -> Maybe (DBText k)) where
  keyName _ = pack $ symbolVal (undefined :: DBText k)
  typeName _ = "TEXT"
instance forall a k b. (DBType b, KnownSymbol k) => DBFieldType (a -> DBForeignKey b k) where
  keyName _ = pack $ symbolVal (undefined :: DBForeignKey b k)
  typeName _ = "INTEGER"
  constraints x =
    [ Data.Text.concat [ "FOREIGN KEY("
                       , keyName x
                       , ") REFERENCES "
                       , name (undefined :: b)
                       , "(rowid)"
                       ]
    ]
instance forall a k b. (DBType b, KnownSymbol k) => DBFieldType (a -> Maybe (DBForeignKey b k)) where
  keyName _ = pack $ symbolVal (undefined :: DBForeignKey b k)
  typeName _ = "INTEGER"
  constraints x =
    [ Data.Text.concat [ "FOREIGN KEY("
                       , keyName x
                       , ") REFERENCES "
                       , name (undefined :: b)
                       , "(rowid)"
                       ]
    ]
