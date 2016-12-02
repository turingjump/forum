{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Forum.Internal.Types where

import           Control.Monad.Except      (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import qualified Data.ByteString           as BS
import           Data.Proxy                (Proxy (..))
import           Data.Reflection           (Reifies (..))
import qualified Data.Text                 as T
import qualified Database.HsSqlPpp.Catalog as Sql
import qualified Database.HsSqlPpp.Types   as Sql
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import           GHC.Word                  (Word16)
import           Hasql.Class               (Decodable, Encodable)
import           Hasql.Pool                (Pool)
import           Hasql.Session             (Error, Session)

newtype PrimaryKey (tbl :: Symbol) val = PrimaryKey val
  deriving (Eq, Show, Read, Generic, Ord)

instance Encodable val => Encodable (PrimaryKey tbl val)
instance Decodable val => Decodable (PrimaryKey tbl val)

newtype ForeignKey (tbl :: Symbol) val = ForeignKey val
  deriving (Eq, Show, Read, Generic, Ord)

instance Encodable val => Encodable (ForeignKey tbl val)
instance Decodable val => Decodable (ForeignKey tbl val)

data DB = DB
  { dbCatalog        :: Sql.Catalog
  , dbSettings       :: DBSettings
  , dbConnectionPool :: Pool
  }

data DBSettings = DBSettings
  { dbName     :: BS.ByteString
  , dbHost     :: BS.ByteString
  , dbPort     :: Word16
  , dbUser     :: BS.ByteString
  , dbPassword :: BS.ByteString
  }

defaultDBSettings :: DBSettings
defaultDBSettings = DBSettings
  { dbName     = "postgres"
  , dbHost     = "localhost"
  , dbPort     = 5432
  , dbUser     = "postgres"
  , dbPassword = "postgres"
  }


-- * SQL types represented as Haskell types

data Scalar (s :: Symbol)

instance KnownSymbol s => Reifies (Scalar s) Sql.Type where
  reflect _ = Sql.ScalarType (T.pack $ symbolVal (Proxy :: Proxy s))

-- * SQL

newtype SQL a = SQL { runSQL' :: Session a }
  -- This isn't quite right. The composition implied by the classes is a little
  -- misleading, since they involve a round-trip through the application, and
  -- encoding and decoding.
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO)
