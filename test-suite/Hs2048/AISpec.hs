module Hs2048.AISpec (spec) where

import           Hs2048.AI
import qualified Hs2048.Direction as D
import           Test.Hspec

spec :: Spec
spec = do
    describe "bestMove" $ do
        it "returns the best move" $ do
            bestMove [[Nothing, Just 2], [Just 2, Nothing]] `shouldBe` D.West

    describe "boards" $ do
        it "returns [] for an empty board" $ do
            boards [] `shouldBe` []

    describe "moves" $ do
        it "returns [] for an empty board" $ do
            moves [] `shouldBe` []

    describe "quality" $ do
        it "returns 0 for an empty board" $ do
            quality [] `shouldBe` 0
