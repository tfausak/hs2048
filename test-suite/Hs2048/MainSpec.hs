module Hs2048.MainSpec (spec) where

import qualified Hs2048.Direction as D
import           Hs2048.Main
import           Test.Hspec

spec :: Spec
spec = do
    describe "direction" $ do
        it "returns Nothing" $ do
            direction "" `shouldBe` Nothing

        it "returns Just West" $ do
            direction "\ESC[D" `shouldBe` Just D.West

        it "returns Just South" $ do
            direction "\ESC[B" `shouldBe` Just D.South

        it "returns Just East" $ do
            direction "\ESC[C" `shouldBe` Just D.East

        it "returns Just North" $ do
            direction "\ESC[A" `shouldBe` Just D.North

    describe "getChars" $ do
        it "is" pending

    describe "getMove" $ do
        it "is" pending

    describe "play" $ do
        it "is" pending
