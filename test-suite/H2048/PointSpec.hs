module H2048.PointSpec (spec) where

import           H2048.Point
import           Test.Hspec

spec :: Spec
spec = do
    describe "x" $ do
        it "returns the first value" $ do
            x (1, 2) `shouldBe` 1

    describe "y" $ do
        it "returns the second value" $ do
            y (1, 2) `shouldBe` 2
