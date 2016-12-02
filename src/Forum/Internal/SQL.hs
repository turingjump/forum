{-# LANGUAGE TemplateHaskell #-}
module Forum.Internal.SQL where

import           Bookkeeper                   ((:=>), All, All2, BKeys, Book',
                                               Const (..), Identity (..), IsEqTo,
                                               bcollapse, bcollapseWithKeys,
                                               bmapConstraint, bproxies)
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
import qualified Database.HsSqlPpp.Parse      as Sql
import qualified Database.HsSqlPpp.Syntax     as Sql
import qualified Database.HsSqlPpp.TypeCheck  as Sql
import qualified Database.HsSqlPpp.Types      as Sql
import qualified Hasql.Class                  as Hasql
import qualified Hasql.Connection             as Hasql
import           Hasql.Pool                   (acquire)
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Quote    as TH
import qualified Language.Haskell.TH.Syntax   as TH

import Forum.Internal.Class
import Forum.Internal.Types

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
      newCtx  <- [t| SqlType $(return $ TH.VarT newBinding) ~ $(return . toHsSqlType $ Sql.teType typ) |]
      return $ (newType, TH.PlainTV newBinding : accBindings, newCtx : accCtx)

    finish :: (TH.Type, [TH.TyVarBndr], TH.Cxt) -> TH.Q TH.Type
    finish (accType, accBindings, accCtx) = do
      let sql = TH.mkName "SQL"
      let typ = TH.AppT (TH.ConT sql) accType
      return $ TH.ForallT accBindings accCtx typ


toHsSqlType :: Sql.Type -> TH.Type
toHsSqlType (Sql.ScalarType s) = TH.LitT . TH.StrTyLit $ T.unpack s
toHsSqlType e = error $ "Only scalar types are currently supported. Saw:\n" ++ show e

-- | Runs type-checking on the statement, and returns the inferred type
typeCheckSQL :: Sql.Statement -> Sql.Catalog -> TH.Q TH.Type
typeCheckSQL s cat = case Sql.typeCheckStatements Sql.defaultTypeCheckFlags cat [s] of
  (_, [typechecked]) -> case typechecked of
    Sql.QueryStatement _ (Sql.Select {..}) -> case Sql.anType ann of
      Just typ' -> toBookType (Sql.teType typ')
        {-let typ  = return . toHsSqlType $ Sql.teType typ'-}
        {-[t| forall queryType . (SqlType queryType ~ $typ) => queryType |]-}
      Nothing -> error $ "Error type-checking SQL: " ++ show typechecked
  (_, _) -> error "Only single statements are currently supported"

makeStatement :: String -> [String] -> TH.Q TH.Exp
makeStatement stmt' params = [e| Hasql.stmtList (BS.pack $stmt) True |]
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
      Right (_, params) -> makeStatement s params
  , TH.quotePat = undefined
  , TH.quoteDec = undefined
  , TH.quoteType = undefined
  }

-- * Creating and deleting DBs

makeTable :: String -> [(T.Text, Sql.Type)] -> BS.ByteString
makeTable tblName columns =
  "CREATE TABLE IF NOT EXISTS " <> BS.pack tblName <> " ( " <> cols <> " ) "
  where
    cols = BS.intercalate ", " (go <$> columns)
    go (colName, Sql.ScalarType colType)
      = T.encodeUtf8 colName <> " " <> T.encodeUtf8 colType


getOrCreateDB
  :: forall (schema :: [*]) f e.
    (BKeys schema, All2 ToTable schema, All (IsEqTo (Book' f e)) schema,  ToCatalogUpdate schema)
  => DBSettings -> Proxy schema -> IO (Either [Sql.TypeError] DB)
getOrCreateDB settings schema = do
  print createTables
  let s = Hasql.settings (dbHost settings)
                         (dbPort settings)
                         (dbUser settings)
                         (dbPassword settings)
                         (dbName settings)
  pool <- acquire (20, 1, s)
  case toCatalog (Proxy :: Proxy (Book' Identity schema)) of
    Left e -> return (Left e)
    Right catalog -> return . Right $ DB
      { dbCatalog        = catalog
      , dbSettings       = settings
      , dbConnectionPool = pool
      }
  where
    schemaProxies :: Book' Proxy schema
    schemaProxies = bproxies

    toFieldProxy :: Proxy (Book' f schema) -> Proxy schema
    toFieldProxy _ = Proxy

    tables' :: Book' (Const [(T.Text, Sql.Type)]) schema
    tables' = bmapConstraint Proxy
                             (Const . toTable . toFieldProxy)
                             schemaProxies

    tables :: [(String, [(T.Text, Sql.Type)])]
    tables  = bcollapseWithKeys tables'

    createTables = uncurry makeTable <$> tables


deleteDB :: DB -> IO ()
deleteDB = undefined
