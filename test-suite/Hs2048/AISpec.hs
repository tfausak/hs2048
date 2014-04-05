module Hs2048.AISpec (spec) where

import           Hs2048.AI
import           Test.Hspec

spec :: Spec
spec = do
    describe "moves" $ do
        it "returns [] for an empty board" $ do
            moves [] `shouldBe` []

    describe "quality" $ do
        it "returns 0 for an empty board" $ do
            quality [] `shouldBe` 0
