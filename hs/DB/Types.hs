{-# LANGUAGE DataKinds,
             ExistentialQuantification,
             FlexibleInstances,
             KindSignatures,
             MultiParamTypeClasses,
             OverloadedStrings,
             ScopedTypeVariables #-}
module DB.Types
( DBInt(DBInt, dbInt)
, DBText(DBText, dbText)
, DBByteString(DBByteString, dbByteString)
, DBDouble(DBDouble, dbDouble)
, DBBool(DBBool, dbBool)
, DBDate(DBDate, dbDate)
, DBKey(DBKey, dbKey)
, DBForeignKey(DBForeignKey, dbForeignKey)
, DBFieldType
, keyName, typeName, createStatement, constraints
, DBField(MkField)
, DBType(mkField, setKey, key, name, fields, namedParams, selectors)
, ToRowDBType(ToRowDBType)
)
where

import qualified Data.ByteString as B
import Data.Int
import Data.Text
import Data.Time.Clock
import Data.Typeable
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import GHC.TypeLits

class (DBType a, ToField f) => DBFieldType a f where
  keyName :: (a -> f) -> Text
  typeName :: (a -> f) -> Text

  createStatement :: (a -> f) -> Text
  createStatement func = Data.Text.concat [keyName func, " ", typeName func]

  constraints :: (a -> f) -> [Text]
  constraints _ = []

  sqlData :: a -> (a -> f) -> SQLData
  sqlData dat func = toField $ func dat

  namedParam :: a -> (a -> f) -> NamedParam
  namedParam dat func = (Data.Text.append "@" $ keyName func) := (func dat)

  selector :: (a -> f) -> Text
  selector func = Data.Text.concat [keyName func, "=@", keyName func]

data DBField a = forall f . DBFieldType a f => MkField (a -> f)
class FromRow a => DBType a where
  mkField :: DBFieldType a f => (a -> f) -> DBField a
  mkField = MkField

  key :: a -> Maybe (DBKey a)
  setKey :: a -> Maybe (DBKey a) -> a
  name :: a -> Text
  fields :: a -> [DBField a]

  namedParams :: a -> [NamedParam]
  namedParams dat = Prelude.map (\(MkField f) -> namedParam dat f) $ fields dat

  selectors :: a -> [Text]
  selectors dat = Prelude.map (\(MkField f) -> selector f) $ fields dat

newtype ToRowDBType a = ToRowDBType a

instance DBType a => ToRow (ToRowDBType a) where
  toRow (ToRowDBType dat) = Prelude.map (\(MkField f) -> sqlData dat f) $ fields dat

newtype DBInt (key :: Symbol) = DBInt { dbInt :: Int }
  deriving (Show, Eq)
newtype DBText (key :: Symbol) = DBText { dbText :: Text }
  deriving (Show, Eq)
newtype DBByteString (key :: Symbol) = DBByteString { dbByteString :: B.ByteString }
  deriving (Show, Eq)
newtype DBDouble (key :: Symbol) = DBDouble { dbDouble :: Double }
  deriving (Show, Eq)
newtype DBBool (key :: Symbol) = DBBool { dbBool :: Bool }
  deriving (Show, Eq)
newtype DBDate (key :: Symbol) = DBDate { dbDate :: UTCTime }
  deriving (Show, Eq)
newtype DBType a => DBKey a = DBKey { dbKey :: Int }
  deriving (Show, Eq)
newtype DBType a => DBForeignKey a (key :: Symbol) = DBForeignKey { dbForeignKey :: DBKey a }
  deriving (Show, Eq)

instance Typeable k => FromField (DBInt k) where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBInt x)
    Errors e -> Errors e
instance Typeable k => FromField (DBText k) where
  fromField field = case (fromField field) :: Ok Text of
    Ok t     -> Ok (DBText t)
    Errors e -> Errors e
instance Typeable k => FromField (DBByteString k) where
  fromField field = case (fromField field) :: Ok B.ByteString of
    Ok b     -> Ok (DBByteString b)
    Errors e -> Errors e
instance Typeable k => FromField (DBDouble k) where
  fromField field = case (fromField field) :: Ok Double of
    Ok d     -> Ok (DBDouble d)
    Errors e -> Errors e
instance Typeable k => FromField (DBBool k) where
  fromField field = case (fromField field) :: Ok Int64 of
    Ok 1     -> Ok (DBBool True)
    Ok 0     -> Ok (DBBool False)
    Ok _     -> returnError ConversionFailed field "Bools must be 0 or 1"
    Errors e -> Errors e
instance Typeable k => FromField (DBDate k) where
  fromField field = case (fromField field) :: Ok UTCTime of
    Ok t     -> Ok (DBDate t)
    Errors e -> Errors e
instance FromField (DBKey a) where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBKey x)
    Errors e -> Errors e
instance (DBType a, Typeable k) => FromField (DBForeignKey a k) where
  fromField field = case (fromField field) :: Ok Int of
    Ok x     -> Ok (DBForeignKey $ DBKey x)
    Errors e -> Errors e

instance ToField (DBInt k) where
  toField (DBInt x) = toField x
instance ToField (DBText k) where
  toField (DBText t) = toField t
instance ToField (DBByteString k) where
  toField (DBByteString b) = toField b
instance ToField (DBDouble k) where
  toField (DBDouble d) = toField d
instance ToField (DBBool k) where
  toField (DBBool b) = toField b
instance ToField (DBDate k) where
  toField (DBDate d) = toField d
instance ToField (DBKey a) where
  toField (DBKey x) = toField x
instance DBType a => ToField (DBForeignKey a k) where
  toField (DBForeignKey (DBKey x)) = toField x

instance DBFieldType a f => DBFieldType a (Maybe f) where
  keyName _ = keyName (undefined :: (a -> f))
  typeName _ = typeName (undefined :: (a -> f))
  createStatement _ = createStatement (undefined :: (a -> f))
  constraints _ = constraints (undefined :: (a -> f))

instance (DBType a, KnownSymbol k) => DBFieldType a (DBInt k) where
  keyName _ = pack $ symbolVal (undefined :: DBInt k)
  typeName _ = "INTEGER"

instance (DBType a, KnownSymbol k) => DBFieldType a (DBText k) where
  keyName _ = pack $ symbolVal (undefined :: DBText k)
  typeName _ = "TEXT"

instance (DBType a, KnownSymbol k) => DBFieldType a (DBByteString k) where
  keyName _ = pack $ symbolVal (undefined :: DBByteString k)
  typeName _ = "BLOB"

instance (DBType a, KnownSymbol k) => DBFieldType a (DBDouble k) where
  keyName _ = pack $ symbolVal (undefined :: DBDouble k)
  typeName _ = "REAL"

instance (DBType a, KnownSymbol k) => DBFieldType a (DBBool k) where
  keyName _ = pack $ symbolVal (undefined :: DBBool k)
  typeName _ = "INTEGER"

instance (DBType a, KnownSymbol k) => DBFieldType a (DBDate k) where
  keyName _ = pack $ symbolVal (undefined :: DBDate k)
  typeName _ = "TEXT"

instance DBType a => DBFieldType a (DBKey a) where
  keyName _ = "rowid"
  typeName _ = "INTEGER"
  createStatement x = Data.Text.concat [keyName x, " ", typeName x, " PRIMARY KEY"]

instance forall a k b. (DBType a, DBType b, KnownSymbol k) => DBFieldType a (DBForeignKey b k) where
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
