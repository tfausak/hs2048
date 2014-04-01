module H2048.SettingsSpec (spec) where

import           H2048.Settings
import           Test.Hspec

spec :: Spec
spec = do
    describe "height" $ do
        it "returns 4" $ do
            height `shouldBe` 4

    describe "maxTile" $ do
        it "returns 2048" $ do
            maxTile `shouldBe` 2048

    describe "tiles" $ do
        it "returns 2" $ do
            tiles `shouldBe` 2

    describe "width" $ do
        it "returns 4" $ do
            width `shouldBe` 4
