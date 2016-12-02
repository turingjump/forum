module Schema where

import Forum
import qualified Data.Text as T
import Data.Int

------------------------------------------------------------------------------
-- Setup

withEmptyDb :: (DB -> IO a) -> IO a
withEmptyDb action = do
  db <- getOrCreateDB "forum-test" (Proxy :: Proxy Schema)
  result <- action db
  deleteDB db
  return result

sql :: QuasiQuoter
sql = sqlQQFor (Proxy :: Proxy Schema)

------------------------------------------------------------------------------
-- Schema and Types

type Species = Book
  '[ "speciesId"  :=> PrimaryKey "species" Int32
   , "name"       :=> T.Text
   , "genus"      :=> ForeignKey "genus" Int32
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int32)
   ]

type Genus = Book
  '[ "genusId"    :=> PrimaryKey "genus" Int32
   , "name"       :=> T.Text
   , "genus"      :=> ForeignKey "family" Int32
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int32)
   ]

type Family = Book
  '[ "familyId"   :=> PrimaryKey "family" Int32
   , "name"       :=> T.Text
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int32)
   ]

type Discoverer = Book
  '[ "discovererId" :=> PrimaryKey "family" Int32
   , "firstname"    :=> T.Text
   , "lastname"     :=> T.Text
   ]

type Schema = Book
  '[ "species"    :=> Species
   , "genus"      :=> Genus
   , "family"     :=> Family
   , "discoverer" :=> Discoverer
   ]
