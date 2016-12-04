{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Forum.Internal.Decoding where

import Bookkeeper
import Hasql.Class (Decodable(decode))
import Hasql.Decoders (Row)
import Data.Proxy

instance (All Decodable entries) => Decodable (Book' Identity entries) where
  decode = decodeBook

decodeBook :: All Decodable entries => Row (Book' Identity entries)
decodeBook = bsequence $ bmapConstraint (Proxy :: Proxy Decodable) decodeProxy bproxies
  where
    decodeProxy :: Decodable a => Proxy a -> Row a
    decodeProxy _ = decode
