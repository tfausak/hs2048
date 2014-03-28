module H2048.VectorSpec (spec) where

import           H2048.Vector
import           Test.Hspec

spec :: Spec
spec = do
    describe "canShift" $ do
        it "returns False for []" $ do
            canShift [] `shouldBe` False

        it "returns False for all empty tiles" $ do
            canShift [Nothing, Nothing] `shouldBe` False

        it "returns True if a tile can be shifted" $ do
            canShift [Nothing, Just 2] `shouldBe` True

        it "returns True if a tile can be combined" $ do
            canShift [Just 2, Just 2] `shouldBe` True

        it "returns False if no tiles can be combined" $ do
            canShift [Just 2, Just 4] `shouldBe` False

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
        it "renders []" $ do
            render [] `shouldBe` ""

        it "renders a vector" $ do
            render [Nothing, Just 2] `shouldBe`
                "\ESC[30m-\ESC[0m\t\ESC[31m2\ESC[0m"

    describe "score" $ do
        it "returns 0 for []" $ do
            score [] `shouldBe` 0

        it "returns 14 for [Nothing, Just 2]" $ do
            score [Nothing, Just 2] `shouldBe` 2

    describe "set" $ do
        it "sets the tile at the index" $ do
            set [Nothing, Nothing] (Just 2) 0 `shouldBe` [Just 2, Nothing]

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
