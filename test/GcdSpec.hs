{-# LANGUAGE DataKinds #-}
module GcdSpec where

import Prelude hiding (gcd)
import Lib (Proxy(..), gcd)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "gcd" $ do
    it "returns gcd of two numbers" $ do
      (gcd 2 2) `shouldBe` 2
      (gcd 0 3) `shouldBe` 3
      (gcd 4 8) `shouldBe` 4
      (gcd 5 6) `shouldBe` 1

    it "does not care about the order of the parameters" $ do
      (gcd 0 3) `shouldBe` (gcd 3 0)
      (gcd 4 8) `shouldBe` (gcd 8 4)
      (gcd 5 6) `shouldBe` (gcd 6 5)
