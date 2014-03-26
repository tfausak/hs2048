module H2048.TileSpec (spec) where

import           H2048.Tile
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "empty" $ do
        it "returns Nothing" $ do
            empty `shouldBe` Nothing

    describe "parse" $ do
        it "returns Nothing for \"-\"" $ do
            parse "-" `shouldBe` Nothing

        prop "returns Just n for \"n\"" $ do
            \ n -> parse (show n) == Just n

    describe "render" $ do
        it "returns \"-\" for Nothing" $ do
            render Nothing `shouldBe` "-"

        prop "returns \"n\" for Just n" $ do
            \ n -> render (Just n) == show n

    describe "score" $ do
        it "returns 0 for Nothing" $ do
            score Nothing `shouldBe` 0

        prop "returns n for Just n" $ do
            \ n -> score (Just n) == n
