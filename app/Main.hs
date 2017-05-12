{-# LANGUAGE DataKinds #-}
module Main where

import Prelude hiding (mod)

import Lib

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
 
