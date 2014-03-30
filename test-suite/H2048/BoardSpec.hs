module H2048.BoardSpec (spec) where

import           H2048.Board
import qualified H2048.Direction as D
import           Test.Hspec

spec :: Spec
spec = do
    describe "canMove" $ do
        it "returns False for []" $ do
            canMove [] D.West `shouldBe` False

        it "can move West" $ do
            canMove [[Nothing, Just 2]] D.West `shouldBe` True

        it "can move South" $ do
            canMove [[Just 2], [Nothing]] D.South `shouldBe` True

        it "can't move East" $ do
            canMove [[Nothing, Just 2]] D.East `shouldBe` False

        it "can't move North" $ do
            canMove [[Just 2], [Nothing]] D.North `shouldBe` False

    describe "canShift" $ do
        it "returns False for []" $ do
            canShift [] `shouldBe` False

        it "returns True if any of the vectors can be shifted" $ do
            canShift [[Nothing, Nothing], [Nothing, Just 2]] `shouldBe` True

    describe "empty" $ do
        it "returns [] for 0 0" $ do
            empty 0 0 `shouldBe` []

        it "returns [[Nothing], [Nothing]] for 1 2" $ do
            empty 1 2 `shouldBe` [[Nothing], [Nothing]]

    describe "emptyPoints" $ do
        it "returns [] for []" $ do
            emptyPoints [] `shouldBe` []

        it "returns the points without tiles" $ do
            emptyPoints [[Nothing, Just 2], [Just 4, Nothing]] `shouldBe`
                [(0, 0), (1, 1)]

    describe "move" $ do
        it "moves West" $ do
            move [[Nothing, Just 2], [Just 4, Nothing]] D.West `shouldBe`
                [[Just 2, Nothing], [Just 4, Nothing]]

        it "moves South" $ do
            move [[Nothing, Just 2], [Just 4, Nothing]] D.South `shouldBe`
                [[Nothing, Nothing], [Just 4, Just 2]]

        it "moves East" $ do
            move [[Nothing, Just 2], [Just 4, Nothing]] D.East `shouldBe`
                [[Nothing, Just 2], [Nothing, Just 4]]

        it "moves North" $ do
            move [[Nothing, Just 2], [Just 4, Nothing]] D.North `shouldBe`
                [[Just 4, Just 2], [Nothing, Nothing]]

    describe "parse" $ do
        it "returns [] for \"\"" $ do
            parse "" `shouldBe` []

        it "returns [[Nothing, Just 2], [Just 4, Just 8]] for \"- 2\n4 8\n\"" $ do
            parse "- 2\n4 8\n" `shouldBe` [[Nothing, Just 2], [Just 4, Just 8]]

    describe "render" $ do
        it "renders []" $ do
            render [] `shouldBe` ""

        it "renders a board" $ do
            render [[Nothing, Just 2], [Just 4, Just 8]] `shouldBe`
                "- 2\n4 8\n"

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

        it "returns 20 for [[Nothing, Just 2], [Just 4, Just 8]]" $ do
            score [[Nothing, Just 2], [Just 4, Just 8]] `shouldBe` 20

    describe "set" $ do
        it "sets the tile at the point" $ do
            set [[Nothing, Nothing], [Nothing, Nothing]] (Just 2) (0, 0) `shouldBe`
                [[Just 2, Nothing], [Nothing, Nothing]]

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
