module H2048.GameSpec (spec) where

import           H2048.Game
import           System.Random (mkStdGen)
import           Test.Hspec

spec :: Spec
spec = do
    describe "randomEmptyIndex" $ do
        it "returns an empty index" $ do
            let g = mkStdGen 0
                v = [Nothing, Nothing]
            fst (randomEmptyIndex v g) `shouldBe` 1

        it "returns an empty index" $ do
            let g = mkStdGen 53668
                v = [Nothing, Nothing]
            fst (randomEmptyIndex v g) `shouldBe` 0

        it "returns a generator" $ do
            let g = mkStdGen 0
            show (snd (randomEmptyIndex [] g)) `shouldBe` "40014 40692"

    describe "randomTile" $ do
        it "returns a 2 tile" $ do
            let g = mkStdGen 0
            fst (randomTile g) `shouldBe` Just 2

        it "returns a 4 tile" $ do
            let g = mkStdGen 1
            fst (randomTile g) `shouldBe` Just 4

        it "returns a generator" $ do
            let g = mkStdGen 0
            show (snd (randomTile g)) `shouldBe` "1601120196 1655838864"
