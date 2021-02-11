module PrepSpec
  ( spec
  ) where

import Prep
import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "noOpProgram" $ do
    it "execute with runPrep returns (empty str, 0)" $ do
      (x, i) <- runPrep False Prep.noOpProgram
      x `shouldBe` ""
      i `shouldBe` 0
