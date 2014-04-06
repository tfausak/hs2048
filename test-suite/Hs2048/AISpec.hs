module Hs2048.AISpec (spec) where

import           Hs2048.AI
import qualified Hs2048.Direction as D
import           Test.Hspec

spec :: Spec
spec = do
    let b =
            [ [Just 2048, Just 1024, Just 512, Just 256]
            , [Just 16, Just 32, Just 64, Just 128]
            , [Just 8, Just 4, Just 2, Just 2]
            , [Nothing, Nothing, Nothing, Nothing]
            ]

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

    describe "roughness" $ do
        it "returns 0 for an empty board" $ do
            roughness [] `shouldBe` 0

        it "calculates the roughness of a board" $ do
            roughness b `shouldBe` 16
