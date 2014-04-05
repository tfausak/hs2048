module Hs2048.AISpec (spec) where

import Test.Hspec
import Hs2048.AI

spec :: Spec
spec = do
    describe "quality" $ do
        it "returns 0 for an empty board" $ do
            quality [] `shouldBe` 0
