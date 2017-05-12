#!/usr/bin/env runhaskell

{-# LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds #-}

import Control.Exception hiding (assert)
import Prelude hiding (mod)
import GHC.TypeLits
import Data.Proxy

data Mod (k :: Nat) = Mod Integer (Proxy k)
  deriving (Show, Eq)

mod :: (KnownNat k) => Integer -> Proxy k -> Mod k
mod n m | n <  natVal m = Mod n m
        | n >= natVal m = let k = natVal m
                          in mod (n-k) m

instance (KnownNat k) => Num (Mod k) where
   (Mod n m) + (Mod n' _) | (n + n') <  natVal m = Mod (n + n')     m
                          | (n + n') >= natVal m = Mod (n + n' - natVal m) m

  
assert :: (Monad m) => Bool -> m ()
assert True  = pure ()
assert False = fail "assertion failed"

assertFail :: a -> IO ()
assertFail a = (evaluate a >> fail "expected exception") `catch` \ (_ :: SomeException) -> return ()  

main :: IO ()
main = do
  let _5 = Proxy :: Proxy 5
  -- tests canonical form
  assert $ 12 `mod` _5 == 2 `mod` _5 
  -- -- tests addition
  assert     $ (2 `mod` _5 + 1 `mod` _5) == 3 `mod` _5
  assert     $ (2 `mod` _5 + 4 `mod` _5) == 1 `mod` _5
  
  -- Does not compile...
  -- 2 `mod` (Proxy :: Proxy 3) + 2 `mod` (Proxy :: Proxy 4)
  -- tests multiplication
 
