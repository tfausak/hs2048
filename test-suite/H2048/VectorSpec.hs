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
