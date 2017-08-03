{-# LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds #-}
module Lib(Mod, Proxy(..), mod, gcd) where

import Prelude hiding (mod, gcd)
import GHC.TypeLits
import Data.Proxy

data Mod (k :: Nat) = Mod Integer (Proxy k)
  deriving (Show, Eq)

mod :: (KnownNat k) => Integer -> Proxy k -> Mod k
mod n m | n < 0         = let k = natVal m
                          in mod (n+k) m
        | n <  natVal m = Mod n m
        | n >= natVal m = let k = natVal m
                          in mod (n-k) m
mod _ _ = undefined

gcd :: Integer -> Integer -> Integer
gcd a b | b == 0 = a
        | otherwise = gcd b (a `rem` b)

instance (KnownNat k) => Num (Mod k) where
   (Mod n m) + (Mod n' _) | (n + n') <  natVal m = Mod (n + n') m
                          | otherwise            = Mod (n + n' - natVal m) m

   (Mod n m) * (Mod n' _) = (n * n') `mod` m

   (Mod n m) - (Mod n' _) = (n - n') `mod` m

