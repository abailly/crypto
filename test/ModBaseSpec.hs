{-# LANGUAGE DataKinds #-}
module ModBaseSpec where

import Prelude hiding (mod)
import Lib (Proxy(..), mod)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec =
  describe "modular addition" $ do
    let _5 = Proxy :: Proxy 5
        _7 = Proxy :: Proxy 7

    it "normalize numbers to canonical form" $ do
      (12 `mod` _5)   `shouldBe` (2 `mod` _5)
      ((-1) `mod` _5) `shouldBe` (4 `mod` _5)

    it "adds two numbers together" $ do
      (3 `mod` _5 + 1 `mod` _5) `shouldBe` (4 `mod` _5)
      (2 `mod` _5 + 4 `mod` _5) `shouldBe` (1 `mod` _5)

    it "multiplies two numbers together" $ do
      (3 `mod` _7 * 2 `mod` _7) `shouldBe` (6 `mod` _7)
      (3 `mod` _5 * 2 `mod` _5) `shouldBe` (1 `mod` _5)

    it "substract two numbers" $ do
      (3 `mod` _5 - 2 `mod` _5) `shouldBe` (1 `mod` _5)
      (3 `mod` _5 - 4 `mod` _5) `shouldBe` (4 `mod` _5)

