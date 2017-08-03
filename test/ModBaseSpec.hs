{-# LANGUAGE DataKinds #-}
module ModBaseSpec where

import Prelude hiding (mod, gcd)
import Lib (Proxy(..), mod, gcd)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
  let _5 = Proxy :: Proxy 5
      _7 = Proxy :: Proxy 7

  describe "canonical form for modular expressions" $ do
    it "normalize numbers to canonical form" $ do
      (12 `mod` _5)   `shouldBe` (2 `mod` _5)
      ((-1) `mod` _5) `shouldBe` (4 `mod` _5)

  describe "modular addition" $ do
    it "adds two numbers together" $ do
      (3 `mod` _5 + 1 `mod` _5) `shouldBe` (4 `mod` _5)
      (2 `mod` _5 + 4 `mod` _5) `shouldBe` (1 `mod` _5)

  describe "modular multiplication" $ do
    it "multiplies two numbers together" $ do
      (3 `mod` _7 * 2 `mod` _7) `shouldBe` (6 `mod` _7)
      (3 `mod` _5 * 2 `mod` _5) `shouldBe` (1 `mod` _5)

  describe "modular substraction" $ do
    it "substract two numbers" $ do
      (3 `mod` _5 - 2 `mod` _5) `shouldBe` (1 `mod` _5)
      (3 `mod` _5 - 4 `mod` _5) `shouldBe` (4 `mod` _5)

  describe "gcd" $ do
    it "returns gcd of two numbers" $ do
      (gcd 2 2) `shouldBe` 2
      (gcd 4 8) `shouldBe` 4
      (gcd 5 6) `shouldBe` 1
    it "does not care about the order of the parameters" $ do
      (gcd 5 6) `shouldBe` (gcd 6 5)
      (gcd 4 8) `shouldBe` (gcd 8 4)
