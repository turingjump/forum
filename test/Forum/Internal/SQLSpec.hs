{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Forum.Internal.SQLSpec (spec) where

import           Database.HsSqlPpp.Dialect (postgresCatalog)
import           Forum
import           Forum.Internal
import           Language.Haskell.TH       (runQ)
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Ppr   as TH
import           Language.Haskell.TH.Alpha (areExpAEq)
import           Test.Hspec

import Schema

spec :: Spec
spec = do
  parseSQLSpec
  typeCheckSQLSpec

parseSQLSpec :: Spec
parseSQLSpec = describe "parseSQL" $ do

  it "returns all params" $ do
    let p = case parseSQL "SELECT * FROM tbl WHERE a = $p1 AND b = $p2;" of
             Left e -> error $ show e
             Right (_, v) -> v
    p `shouldBe` ["p1", "p2"]

typeCheckSQLSpec :: Spec
typeCheckSQLSpec = describe "typeCheckSQL" $ do

  it "returns the expected type" $ do
    -- In order to not have to worry about alpha conversion, we just check that
    -- something with the type we expect typechecks as the type we get
    {-let expected :: forall i j. (SqlType i ~ Scalar "varchar", SqlType j ~ Scalar "varchar") => SQL '[ "firstname" :=> i, "lastname" :=> j]-}
        {-expected = undefined-}

    let asExp typ = TH.SigE (TH.VarE (TH.mkName "test")) typ
    {-expected <- runQ [t| forall i j. (SqlType i ~ Scalar "varchar", SqlType j ~ Scalar "varchar") => SQL '[ "firstname" :=> i, "lastname" :=> j ] |]-}
    expected <- runQ [t| forall i j. (SqlType i ~ Scalar "varchar", SqlType j ~ Scalar "varchar") => SQL '[ "firstname" :=> i, "lastname" :=> j ] |]
    let catalog = case toCatalog (Proxy :: Proxy Schema) of
          Left e -> error $ show e
          Right v -> v
    case parseSQL "SELECT firstname, lastname FROM discoverer;" of
      Right ([v], _) -> do
        print =<< (TH.ppr <$> runQ (typeCheckSQL v catalog))
        print expected
        result <- (asExp <$> typeCheckSQL v catalog) `areExpAEq` (return $ asExp expected)
        result `shouldBe` True
      Left e -> error $ show e
