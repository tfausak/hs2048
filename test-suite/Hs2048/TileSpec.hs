module Hs2048.TileSpec (spec) where

import           Hs2048.Tile
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "empty" $ do
        it "is Nothing" $ do
            empty `shouldBe` Nothing

    describe "parse" $ do
        it "parses \"-\"" $ do
            parse "-" `shouldBe` Nothing

        prop "parses \"n\"" $ \ n ->
            parse (show n) == Just n

    describe "rank" $ do
        it "ranks Nothing" $ do
            rank Nothing `shouldBe` 0

        prop "ranks Just n" $ \ n ->
            rank (Just n) == floor (logBase (2 :: Double) (fromIntegral n))

    describe "render" $ do
        it "renders Nothing" $ do
            render Nothing `shouldBe` "-"

        prop "renders Just n" $ \ n ->
            render (Just n) == show n

    describe "score" $ do
        it "scores Nothing" $ do
            score Nothing `shouldBe` 0

        prop "scores Just n" $ \ n ->
            score (Just n) == n * (rank (Just n) - 1)
