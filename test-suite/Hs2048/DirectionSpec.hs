module Hs2048.DirectionSpec (spec) where

import           Hs2048.Direction
import           Test.Hspec

spec :: Spec
spec = do
    describe "Direction" $ do
        it "is an instance of Bounded" $ do
            minBound `shouldBe` West
            maxBound `shouldBe` North

        it "is an instance of Enum" $ do
            succ West `shouldBe` South
            pred North `shouldBe` East
            toEnum 1 `shouldBe` South
            fromEnum East `shouldBe` 2
            enumFrom North `shouldBe` [North]
            enumFromThen West East `shouldBe` [West, East]
            enumFromTo West West `shouldBe` [West]
            enumFromThenTo West East North `shouldBe` [West, East]

        it "is an instance of Eq" $ do
            West == West `shouldBe` True
            West /= West `shouldBe` False

        it "is an instance of Show" $ do
            show West `shouldBe` "West"
            showList [West] "" `shouldBe` "[West]"

    describe "directions" $ do
        it "returns the directions" $ do
            directions `shouldBe` [West, South, East, North]

    describe "render" $ do
        it "renders West" $ do
            render West `shouldBe` "\x2190"

        it "renders South" $ do
            render South `shouldBe` "\x2193"

        it "renders East" $ do
            render East `shouldBe` "\x2192"

        it "renders North" $ do
            render North `shouldBe` "\x2191"
