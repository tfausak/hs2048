module H2048.GameSpec (spec) where

import           H2048.Game
import           System.Random (mkStdGen)
import           Test.Hspec

spec :: Spec
spec = do
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
