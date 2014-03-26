module H2048.GridSpec (spec) where

import qualified H2048.Direction as D
import           H2048.Grid
import           Test.Hspec

spec :: Spec
spec = do
    describe "empty" $ do
        it "returns [] for 0 0" $ do
            empty 0 0 `shouldBe` []

        it "returns [[Nothing], [Nothing]] for 1 2" $ do
            empty 1 2 `shouldBe` [[Nothing], [Nothing]]

    describe "parse" $ do
        it "returns [] for \"\"" $ do
            parse "" `shouldBe` []

        it "returns [[Nothing, Just 2], [Just 4, Just 8]] for \"- 2\n4 8\n\"" $ do
            parse "- 2\n4 8\n" `shouldBe` [[Nothing, Just 2], [Just 4, Just 8]]

    describe "render" $ do
        it "returns \"\" for []" $ do
            render [] `shouldBe` ""

        it "returns \"- 2\n4 8\n\" for [[Nothing, Just 2], [Just 4, Just 8]]" $ do
            render [[Nothing, Just 2], [Just 4, Just 8]] `shouldBe` "- 2\n4 8\n"

    describe "rotate" $ do
        it "returns [] for []" $ do
            rotate [] `shouldBe` []

        it "rotates clockwise" $ do
            rotate [[Nothing, Just 2], [Just 4, Just 8]] `shouldBe`
                [[Just 4, Nothing], [Just 8, Just 2]]

    describe "rotateFrom" $ do
        it "rotates West" $ do
            rotateFrom [[Nothing, Just 2], [Just 4, Just 8]] D.West `shouldBe`
                [[Nothing, Just 2], [Just 4, Just 8]]

        it "rotates South" $ do
            rotateFrom [[Nothing, Just 2], [Just 4, Just 8]] D.South `shouldBe`
                [[Just 2, Just 8], [Nothing, Just 4]]

        it "rotates East" $ do
            rotateFrom [[Nothing, Just 2], [Just 4, Just 8]] D.East `shouldBe`
                [[Just 8, Just 4], [Just 2, Nothing]]

        it "rotates North" $ do
            rotateFrom [[Nothing, Just 2], [Just 4, Just 8]] D.North `shouldBe`
                [[Just 4, Nothing], [Just 8, Just 2]]

    describe "rotateTo" $ do
        it "rotates West" $ do
            rotateTo [[Nothing, Just 2], [Just 4, Just 8]] D.West `shouldBe`
                [[Nothing, Just 2], [Just 4, Just 8]]

        it "rotates South" $ do
            rotateTo [[Nothing, Just 2], [Just 4, Just 8]] D.South `shouldBe`
                [[Just 4, Nothing], [Just 8, Just 2]]

        it "rotates East" $ do
            rotateTo [[Nothing, Just 2], [Just 4, Just 8]] D.East `shouldBe`
                [[Just 8, Just 4], [Just 2, Nothing]]

        it "rotates North" $ do
            rotateTo [[Nothing, Just 2], [Just 4, Just 8]] D.North `shouldBe`
                [[Just 2, Just 8], [Nothing, Just 4]]

    describe "score" $ do
        it "returns 0 for []" $ do
            score [] `shouldBe` 0

        it "returns 14 for [[Nothing, Just 2], [Just 4, Just 8]]" $ do
            score [[Nothing, Just 2], [Just 4, Just 8]] `shouldBe` 14

    describe "shift" $ do
        it "returns [] for []" $ do
            shift [] `shouldBe` []

        it "shifts toward the heads" $ do
            shift [[Nothing, Just 2], [Nothing, Just 4]] `shouldBe`
                [[Just 2, Nothing], [Just 4, Nothing]]

        it "combines like tiles" $ do
            shift [[Just 2, Just 2]] `shouldBe` [[Just 4, Nothing]]

        it "only combines pairs" $ do
            shift [[Just 2, Just 2, Just 2, Just 2]] `shouldBe`
                [[Just 4, Just 4, Nothing, Nothing]]
