{-# LANGUAGE TemplateHaskell #-}
module Forum.Internal.SQL where

import           Bookkeeper                   ((:=>), Book', Identity, Book, sorted)
import           Control.Monad                (foldM)
import qualified Data.ByteString.Char8        as BS
import           Data.Char                    (isAlphaNum)
import           Data.Monoid                  ((<>))
import           Data.Proxy                   (Proxy (Proxy))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as LT
import qualified Database.HsSqlPpp.Annotation as Sql
import qualified Database.HsSqlPpp.Catalog    as Sql
import qualified Database.HsSqlPpp.Dialect    as Sql (postgresCatalog)
import qualified Database.HsSqlPpp.Parse      as Sql
import qualified Database.HsSqlPpp.Syntax     as Sql
import qualified Database.HsSqlPpp.TypeCheck  as Sql
import qualified Database.HsSqlPpp.Types      as Sql
import qualified Hasql.Class                  as Hasql
import qualified Hasql.Connection             as Hasql
import           Hasql.Pool                   (UsageError, acquire, use)
import qualified Hasql.Query                  as Hasql
import qualified Hasql.Session                as Hasql
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Quote    as TH
import qualified Language.Haskell.TH.Syntax   as TH

import Forum.Internal.Class
import Forum.Internal.Types

runSQL :: Hasql.Decodable (Book b) => DB -> SQL b -> IO (Either UsageError [Book b])
runSQL db (SQL query) = use (dbConnectionPool db) (Hasql.query () query)

parseSQL :: String -> Either Sql.ParseErrorExtra ([Sql.Statement], [String])
parseSQL s = (, params) <$> parsed
  where
    parsed = Sql.parseStatements Sql.defaultParseFlags
                                 "" Nothing (LT.pack . unwords $ stmt)
    go ('$':word) (stmt', params')
      = (('?' : extra) : stmt' , param : params')
         where
           (param, extra) = span isAlphaNum word
    go word (stmt', params')
      = (word : stmt', params')
    (stmt, params) = foldr go ([], []) (words s)

-- | Construct the book type corresponding to the typechecked SQL type.
-- This is not the type people see, but rather the type that's used for
-- decoding. The difference is that the type people see is sorted (i.e., a
-- proper @Book@), whereas this type is in the same order as the columns
-- selected from the table.
toBookType :: Sql.Type -> TH.Q TH.Type
toBookType (Sql.CompositeType ts) = finish =<< foldM go original ts
  where
    original :: (TH.Type, [TH.TyVarBndr], TH.Cxt)
    original = (TH.PromotedNilT, [], [])

    go :: (TH.Type, [TH.TyVarBndr], TH.Cxt) -> (T.Text, Sql.TypeExtra)
        -> TH.Q (TH.Type, [TH.TyVarBndr], TH.Cxt)
    go (accType, accBindings, accCtx) (col, typ) = do
      newBinding <- TH.newName "queryType"
      let colSymbol = TH.LitT . TH.StrTyLit $ T.unpack col
      newType <- [t| $(return colSymbol) :=> $(return $ TH.VarT newBinding) ': $(return accType) |]
      newCtx  <- [t| SqlType $(return $ TH.VarT newBinding) ~ $(toHsSqlType $ Sql.teType typ) |]
      return $ (newType, TH.PlainTV newBinding : accBindings, newCtx : accCtx)

    finish :: (TH.Type, [TH.TyVarBndr], TH.Cxt) -> TH.Q TH.Type
    finish (accType, accBindings, accCtx) = do
      typ <- [t| Hasql.Query () [Book' Identity $(return accType)] |]
      return $ TH.ForallT accBindings accCtx typ

    toHsSqlType :: Sql.Type -> TH.Q TH.Type
    toHsSqlType (Sql.ScalarType s) = [t| Scalar $(return . TH.LitT . TH.StrTyLit $ T.unpack s) |]
    toHsSqlType e = error $ "Only scalar types are currently supported. Saw:\n" ++ show e

-- | Runs type-checking on the statement, and returns the inferred type
typeCheckSQL :: Sql.Statement -> Sql.Catalog -> TH.Q TH.Type
typeCheckSQL s cat = case Sql.typeCheckStatements Sql.defaultTypeCheckFlags cat [s] of
  (_, [typechecked]) -> case typechecked of
    Sql.QueryStatement _ (Sql.Select {..}) -> case Sql.anType ann of
      Just typ' -> toBookType (Sql.teType typ')
      Nothing -> error $ "Error type-checking SQL: " ++ show typechecked
  (_, _) -> error "Only single statements are currently supported"

makeStatement :: TH.Q TH.Type -> String -> [String] -> TH.Q TH.Exp
makeStatement returnType stmt' params =
  [e| let e :: $(returnType)
          e = Hasql.stmtList (BS.pack $stmt) True
      in  (SQL $ fmap sorted <$> e)
  |]
  where
    stmt = TH.liftString stmt'

sqlQQFor :: forall a f. ToCatalogUpdate a => Proxy (Book' f (a :: [*])) -> TH.QuasiQuoter
sqlQQFor p = case sqlQQForSchema <$> toCatalog p of
  Left e -> error $ "Error constructing catalog: " ++ show e
  Right v -> v

sqlQQForSchema :: Sql.Catalog -> TH.QuasiQuoter
sqlQQForSchema catalog = TH.QuasiQuoter
  { TH.quoteExp = \s -> case parseSQL s of
      Left err -> error $ show err
      Right ([parsed], params) -> makeStatement (typeCheckSQL parsed catalog) s params
  , TH.quotePat = undefined
  , TH.quoteDec = undefined
  , TH.quoteType = undefined
  }

-- * Creating and deleting DBs


catalogUpdateToSql :: Sql.CatalogUpdate -> BS.ByteString
catalogUpdateToSql (Sql.CatCreateTable (schema, tblName) columns) =
  "CREATE TABLE IF NOT EXISTS " <> T.encodeUtf8 tblName <> " ( " <> cols <> " ) "
  where
    cols = BS.intercalate ", " (go <$> columns)
    go (colName, colType)
      = T.encodeUtf8 colName <> " " <> T.encodeUtf8 (Sql.catName colType)

-- | Creates a database if it does not yet exist.
--   Creates the tables if they do not yet exist.
getOrCreateDB
  :: forall f schema . (ToCatalogUpdate schema)
  => DBSettings -> Proxy (Book' f schema) -> IO (Either String DB)
getOrCreateDB settings schema = do

  let s = Hasql.settings (dbHost settings)
                         (dbPort settings)
                         (dbUser settings)
                         (dbPassword settings)
                         (dbName settings)
  print s
  {-callProcess "createdb" [ "-p", show (dbPort settings)-}
                         {--- TODO: host (somehow 'localhost'/127.0.0.1 isn't working)-}
                         {-[>, "-h", BS.unpack (dbHost settings)<]-}
                         {-[>, "--no-password"<]-}
                         {-[>, "-U", BS.unpack (dbUser settings)<]-}
                         {-, BS.unpack (dbName settings)-}
                         {-]-}
  pool <- acquire (20, 1, s)
  let catalog = toCatalogUpdate (Proxy :: Proxy schema)
  let setup = BS.intercalate ";\n" $ catalogUpdateToSql <$> catalog
  let mentireCatalog = Sql.updateCatalog catalog Sql.postgresCatalog
  print setup
  case mentireCatalog of
    Left e -> return $ Left (show e)
    Right entireCatalog -> do
      setupResult <- use pool (Hasql.sql setup)
      print setupResult
      case setupResult of
        Left e -> return $ Left (show e)
        Right () -> return . Right $ DB
          { dbCatalog        = entireCatalog
          , dbSettings       = settings
          , dbConnectionPool = pool
          }

deleteDB :: DB -> IO ()
deleteDB = undefined
