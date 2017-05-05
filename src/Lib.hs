#!/usr/bin/env runhaskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception hiding (assert)
import Prelude hiding (mod)

data Mod = Mod Int Int
  deriving (Show, Eq)

data IncompatibleMod = IncompatibleMod
  deriving (Show)

instance Exception IncompatibleMod

mod :: Int -> Int -> Mod
mod n m | n <  m = Mod n m
        | n >= m = mod (n-m) m

instance Num Mod where
  (Mod n m) + (Mod n' m') | m /= m'       = throw IncompatibleMod
                          | (n + n') < m  = Mod (n + n')     m
                          | (n + n') >= m = Mod (n + n' - m) m

  
assert :: (Monad m) => Bool -> m ()
assert True  = pure ()
assert False = fail "assertion failed"

assertFail :: a -> IO ()
assertFail a = (evaluate a >> fail "expected exception") `catch` \ (_ :: IncompatibleMod) -> return ()  

main :: IO ()
main = do
  -- tests canonical form
  assert $ 12 `mod` 5 == 2 `mod` 5 
  -- tests addition
  assert     $ (2 `mod` 5 + 1 `mod` 5) == 3 `mod` 5
  assert     $ (2 `mod` 5 + 4 `mod` 5) == 1 `mod` 5
  assertFail $ (2 `mod` 3 + 2 `mod` 4) 
  -- tests multiplication
 
