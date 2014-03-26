module H2048.VectorSpec (spec) where

import           H2048.Vector
import           Test.Hspec

spec :: Spec
spec = do
    describe "empty" $ do
        it "returns [] for 0" $ do
            empty 0 `shouldBe` []

        it "returns [Nothing, Nothing] for 2" $ do
            empty 2 `shouldBe` [Nothing, Nothing]

    describe "emptyIndexes" $ do
        it "returns [] for []" $ do
            emptyIndexes [] `shouldBe` []

        it "returns the indexes without tiles" $ do
            emptyIndexes [Nothing, Just 2, Nothing, Just 4] `shouldBe` [0, 2]

    describe "parse" $ do
        it "returns [] for \"\"" $ do
            parse "" `shouldBe` []

        it "returns [Nothing, Just 2] for \"- 2\"" $ do
            parse "- 2" `shouldBe` [Nothing, Just 2]

    describe "render" $ do
        it "returns \"\" for []" $ do
            render [] `shouldBe` ""

        it "returns \"- 2\" for [Nothing, Just 2]" $ do
            render [Nothing, Just 2] `shouldBe` "- 2"

    describe "score" $ do
        it "returns 0 for []" $ do
            score [] `shouldBe` 0

        it "returns 14 for [Nothing, Just 2]" $ do
            score [Nothing, Just 2] `shouldBe` 2

    describe "shift" $ do
        it "returns [] for []" $ do
            shift [] `shouldBe` []

        it "shifts toward the head" $ do
            shift [Nothing, Just 2] `shouldBe` [Just 2, Nothing]

        it "combines like tiles" $ do
            shift [Just 2, Just 2] `shouldBe` [Just 4, Nothing]

        it "only combines pairs" $ do
            shift [Just 2, Just 2, Just 2, Just 2] `shouldBe`
                [Just 4, Just 4, Nothing, Nothing]
