module H2048.MainSpec (spec) where

import qualified H2048.Direction as D
import           H2048.Main
import           Test.Hspec

spec :: Spec
spec = do
    describe "direction" $ do
        it "returns West" $ do
            direction 'D' `shouldBe` Just D.West

        it "returns South" $ do
            direction 'B' `shouldBe` Just D.South

        it "returns East" $ do
            direction 'C' `shouldBe` Just D.East

        it "returns North" $ do
            direction 'A' `shouldBe` Just D.North

        it "returns Nothing" $ do
            direction '?' `shouldBe` Nothing

    describe "getMove" $ do
        it "is" pending

    describe "play" $ do
        it "is" pending
