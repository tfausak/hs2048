module H2048.PointSpec (spec) where

import           H2048.Point
import           Test.Hspec

spec :: Spec
spec = do
    let p = (1, 2)

    describe "x" $ do
        it "returns the first value" $ do
            x p `shouldBe` 1

    describe "y" $ do
        it "returns the second value" $ do
            y p `shouldBe` 2
