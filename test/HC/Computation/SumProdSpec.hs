module HC.Computation.SumProdSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import System.Random

import HC.Computation.SumProd (sumprod)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sum of products" $ do
    it "0 is null" $ do
      sumprod 0 >>= (`shouldBe` 0.0)
    it "the more the better" $ do
      sumprod 42 >>= (`shouldSatisfy` (> 0.0))
    it "consecutive calls are equal" $ do
      setStdGen $ mkStdGen 77
      sumx1 <- sumprod 42
      setStdGen $ mkStdGen 77
      sumx2 <- sumprod 42
      sumx1 `shouldBe` sumx2

