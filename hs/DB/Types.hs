{-# LANGUAGE OverloadedStrings,
             DataKinds,
             KindSignatures,
             DeriveDataTypeable,
             ScopedTypeVariables,
             FlexibleInstances #-}
module DB.Types
( DBInt(DBInt, dbInt), DBText(DBText, dbText)
, DBFieldType
, keyName, typeName
)
where

import Data.Int
import Data.Text
import Data.Typeable
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import GHC.TypeLits

newtype DBInt (key :: Symbol) = DBInt { dbInt :: Int }
  deriving Show
newtype DBText (key :: Symbol) = DBText { dbText :: Text }
  deriving Show

instance Typeable k => FromField (DBInt k) where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBInt x)
    Errors e -> Errors e
instance Typeable k => FromField (DBText k) where
  fromField field = case (fromField field) :: Ok Text of
    Ok t     -> Ok (DBText t)
    Errors e -> Errors e

class DBFieldType a where
  keyName :: a -> Text
  typeName :: a -> Text

instance KnownSymbol k => DBFieldType (a -> DBInt k) where
  keyName _ = pack $ symbolVal (undefined :: DBInt k)
  typeName _ = "INTEGER"
instance KnownSymbol k => DBFieldType (a -> DBText k) where
  keyName _ = pack $ symbolVal (undefined :: DBText k)
  typeName _ = "TEXT"
