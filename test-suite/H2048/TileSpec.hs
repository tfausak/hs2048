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
        it "renders Nothing" $ do
            render Nothing `shouldBe` "\ESC[30m-\ESC[0m"

        it "renders Just 2" $ do
            render (Just 2) `shouldBe` "\ESC[31m2\ESC[0m"

        it "renders Just 4" $ do
            render (Just 4) `shouldBe` "\ESC[32m4\ESC[0m"

        it "renders Just 2048" $ do
            render (Just 2048) `shouldBe` "\ESC[45m2048\ESC[0m"

    describe "score" $ do
        it "returns 0 for Nothing" $ do
            score Nothing `shouldBe` 0

        prop "returns n for Just n" $ do
            \ n -> score (Just n) == n
