{-# LANGUAGE QuasiQuotes #-}
module ForumSpec (spec) where

import Forum
import Schema
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = describe "forum" $ around withEmptyDb $ do
  let discoverer :: Discoverer
      discoverer = emptyBook & #discovererId =: PrimaryKey 1
                             & #firstname =: "Carl"
                             & #lastname =: "Linnaeus"

  it "allows querying" $ \db -> do
    Right result <- runSQL db [sql| SELECT firstname FROM discoverer; |]
    result `shouldBe` ([] :: [Book '["firstName" :=> T.Text]])

  {-it "allows inserting" $ \db -> do-}
    {-runSql [sql| INSERT INTO discoverer VALUES $discoverer; |]-}
    {-result <- runSql [sql| SELECT firstName FROM discoverer; |]-}
    {-result `shouldBe` [subSet discoverer]-}

  {-it "allows WHERE clauses" $ \db -> do-}
    {-runSql [sql| INSERT INTO discoverer VALUES $discoverer; |]-}
    {-result <- runSql [sql| SELECT firstName FROM discoverer WHERE lastName = 0; |]-}
    {-result `shouldBe` []-}

  {-it "types WHERE on primary keys as Maybe" $ \db -> do-}
    {-runSql [sql| INSERT INTO discoverer VALUES $discoverer; |]-}
    {-result <- runSql [sql| SELECT firstName FROM discoverer WHERE discovererId = 0; |]-}
    {-result `shouldBe` Nothing-}
