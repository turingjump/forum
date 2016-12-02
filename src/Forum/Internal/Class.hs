module Forum.Internal.Class where

import           Bookkeeper
import           Data.Bifunctor            (second)
import           Data.Int
import           Data.Proxy                (Proxy (..))
import           Data.Reflection           (Reifies (..))
import qualified Data.Text                 as T
import qualified Database.HsSqlPpp.Catalog as Sql
import qualified Database.HsSqlPpp.Types   as Sql
import qualified Database.HsSqlPpp.Dialect as Sql (postgresCatalog)
import           GHC.TypeLits

import Forum.Internal.Types

-- * HasSqlValue

class (Reifies (SqlType haskellType) Sql.Type) => HasSqlValue (haskellType :: *) where
  type SqlType haskellType
  type IsNullable haskellType :: Bool
  type IsNullable haskellType = 'False

instance HasSqlValue T.Text where type SqlType T.Text = Scalar "varchar"
instance HasSqlValue String where type SqlType String = Scalar "varchar"
instance HasSqlValue Bool   where type SqlType Bool   = Scalar "bool"
instance HasSqlValue Int16  where type SqlType Int16  = Scalar "int2"
instance HasSqlValue Int32  where type SqlType Int32  = Scalar "int4"
instance HasSqlValue Int64  where type SqlType Int64  = Scalar "int8"
instance HasSqlValue Float  where type SqlType Float  = Scalar "float4"
instance HasSqlValue Double where type SqlType Double = Scalar "float8"
instance (HasSqlValue a, IsNullable a ~ 'False) => HasSqlValue (Maybe a) where
  type SqlType (Maybe a) = SqlType a
  type IsNullable (Maybe a) = 'True

instance (HasSqlValue a, IsNullable a ~ 'False) => HasSqlValue (PrimaryKey key a) where
  type SqlType (PrimaryKey key a) = SqlType a
  type IsNullable (PrimaryKey key a) = 'False

instance (HasSqlValue a) => HasSqlValue (ForeignKey key a) where
  type SqlType (ForeignKey key a) = SqlType a
  type IsNullable (ForeignKey key a) = IsNullable a

toSqlType :: forall a. HasSqlValue a => Proxy a -> Sql.Type
toSqlType _ = reflect (Proxy :: Proxy (SqlType a))

-- * ToTable

toTable' :: forall entries f. (BKeys entries, All HasSqlValue entries) =>
  Proxy (Book' f entries) -> [(String, Sql.Type)]
toTable' _ = bcollapseWithKeys $ bmapConstraint (Proxy :: Proxy HasSqlValue) go b
  where
    go :: forall a. HasSqlValue a => f a -> Const Sql.Type a
    go _ = Const $ toSqlType (Proxy :: Proxy a)

    b :: Book' f entries
    b = undefined

class ToTable (a :: [*]) where
  toTable :: Proxy a -> [(T.Text, Sql.Type)]

instance (HasSqlValue fieldVal, KnownSymbol fieldName)
  => ToTable '[ fieldName :=> fieldVal ] where
  toTable _ = [(T.pack $ symbolVal (Proxy :: Proxy fieldName)
              , toSqlType (Proxy :: Proxy fieldVal))]

instance (HasSqlValue fieldVal, ToTable (snd ': restOfTable), KnownSymbol fieldName)
  => ToTable (fieldName :=> fieldVal ': snd ': restOfTable) where
  toTable _
    = ( T.pack $ symbolVal (Proxy :: Proxy fieldName)
      , toSqlType (Proxy :: Proxy fieldVal))
     : toTable (Proxy :: Proxy (snd ': restOfTable))

-- * ToCatalogUpdate

toCatalog :: forall a f. ToCatalogUpdate a => Proxy (Book' f a) -> Either [Sql.TypeError] Sql.Catalog
toCatalog _ = Sql.updateCatalog (toCatalogUpdate p) Sql.postgresCatalog
  where
    p :: Proxy a
    p = Proxy

class ToCatalogUpdate (a :: [*]) where
  toCatalogUpdate :: Proxy a -> [Sql.CatalogUpdate]

instance {-# OVERLAPPING #-} ToCatalogUpdate '[] where
  toCatalogUpdate _ = []

instance {-# OVERLAPPABLE #-}
  (KnownSymbol tableName, ToTable table, ToCatalogUpdate rest)
  => ToCatalogUpdate ( tableName :=> Book' f table ': rest) where
  toCatalogUpdate _ = Sql.CatCreateTable tableName columns
                    : toCatalogUpdate (Proxy :: Proxy rest)
    where
      tableName = ("public", T.pack $ symbolVal (Proxy :: Proxy tableName))

      columns  = second typeToCatNameExtra <$> toTable (Proxy :: Proxy table)

      typeToCatNameExtra :: Sql.Type -> Sql.CatNameExtra
      typeToCatNameExtra (Sql.ScalarType t) = Sql.mkCatNameExtra t
