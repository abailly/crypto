{-# LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds #-}
module Lib(Mod, Proxy(..), mod) where

import Prelude hiding (mod)
import GHC.TypeLits
import Data.Proxy

data Mod (k :: Nat) = Mod Integer (Proxy k)
  deriving (Show, Eq)

mod :: (KnownNat k) => Integer -> Proxy k -> Mod k
mod n m | n <  natVal m = Mod n m
        | n >= natVal m = let k = natVal m
                          in mod (n-k) m
mod _ _ = undefined

instance (KnownNat k) => Num (Mod k) where
   (Mod n m) + (Mod n' _) | (n + n') <  natVal m = Mod (n + n') m
                          | (n + n') >= natVal m = Mod (n + n' - natVal m) m
   _ + _ = undefined 
