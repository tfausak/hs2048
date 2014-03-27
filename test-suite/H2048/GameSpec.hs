module H2048.GameSpec (spec) where

import           H2048.Game
import           System.Random (mkStdGen)
import           Test.Hspec

spec :: Spec
spec = do
    describe "addRandomTile" $ do
        it "returns id for []" $ do
            fst (addRandomTile [] (mkStdGen 0)) `shouldBe` []

        it "adds a random tile" $ do
            fst (addRandomTile [[Nothing]] (mkStdGen 0)) `shouldBe` [[Just 2]]

        it "returns id when there are no empty tiles" $ do
            fst (addRandomTile [[Just 2]] (mkStdGen 0)) `shouldBe`
                [[Just 2]]

    describe "addRandomTiles" $ do
        it "returns id for 0" $ do
            fst (addRandomTiles 0 [[Nothing]] (mkStdGen 0)) `shouldBe`
                [[Nothing]]

        it "adds a random tile" $ do
            fst (addRandomTiles 1 [[Nothing]] (mkStdGen 0)) `shouldBe`
                [[Just 2]]

        it "adds some random tiles" $ do
            fst (addRandomTiles 2 [[Nothing, Nothing]] (mkStdGen 0)) `shouldBe`
                [[Just 2, Just 2]]

        it "returns id when there are no empty tiles" $ do
            fst (addRandomTiles 1 [[Just 2]] (mkStdGen 0)) `shouldBe`
                [[Just 2]]

    describe "isOver" $ do
        it "returns True for []" $ do
            isOver [] `shouldBe` True

        it "returns False if there are empty tiles" $ do
            isOver [[Nothing]] `shouldBe` False

        it "returns False if there are moves" $ do
            isOver [[Just 2, Just 2]] `shouldBe` False

        it "returns True if there are no empty tiles and no moves" $ do
            isOver [[Just 2]] `shouldBe` True

    describe "randomEmptyIndex" $ do
        it "returns Nothing for []" $ do
            fst (randomEmptyIndex [] (mkStdGen 0)) `shouldBe` Nothing

        it "returns an empty index" $ do
            let g = mkStdGen 0
                v = [Nothing, Nothing]
            fst (randomEmptyIndex v g) `shouldBe` Just 1

        it "returns an empty index" $ do
            let g = mkStdGen 53668
                v = [Nothing, Nothing]
            fst (randomEmptyIndex v g) `shouldBe` Just 0

    describe "randomEmptyPoint" $ do
        it "returns Nothing for []" $ do
            fst (randomEmptyPoint [] (mkStdGen 0)) `shouldBe` Nothing

        it "returns an empty point" $ do
            let g = mkStdGen 0
                b = [[Nothing, Nothing], [Nothing, Nothing]]
            fst (randomEmptyPoint b g) `shouldBe` Just (0, 1)

        it "returns an empty point" $ do
            let g = mkStdGen 2
                b = [[Nothing, Nothing], [Nothing, Nothing]]
            fst (randomEmptyPoint b g) `shouldBe` Just (1, 1)

    describe "randomTile" $ do
        it "returns a 2 tile" $ do
            let g = mkStdGen 0
            fst (randomTile g) `shouldBe` Just 2

        it "returns a 4 tile" $ do
            let g = mkStdGen 1
            fst (randomTile g) `shouldBe` Just 4
