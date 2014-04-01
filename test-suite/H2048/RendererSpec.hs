module H2048.RendererSpec (spec) where

import           H2048.Renderer
import           Test.Hspec

spec :: Spec
spec = do
    describe "center" $ do
        it "centers a string" $ do
            center 3 "x" `shouldBe` " x "

    describe "color" $ do
        it "returns the color code" $ do
            color Nothing `shouldBe` 30

        it "returns the color code" $ do
            color (Just 2048) `shouldBe` 45

    describe "renderBoard" $ do
        it "renders a board" $ do
            renderBoard [[Nothing, Just 16]] `shouldBe`
                " \ESC[30m-\ESC[0m \ESC[34m16\ESC[0m\n"

    describe "renderGame" $ do
        it "renders a game" $ do
            renderGame [[Nothing, Just 2]] `shouldBe`
                "Score: 0\n\ESC[30m-\ESC[0m \ESC[31m2\ESC[0m\n"

    describe "renderTile" $ do
        it "renders a tile" $ do
            renderTile Nothing `shouldBe` "\ESC[30m-\ESC[0m"
