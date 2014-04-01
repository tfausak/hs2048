module Hs2048.VectorSpec (spec) where

import qualified Hs2048.Tile           as T
import           Hs2048.Vector
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    let v1 = replicate 4 Nothing
        v2 = replicate 4 (Just 2)

    describe "canShift" $ do
        it "returns false if the vector can't be shifted" $ do
            canShift v1 `shouldBe` False

        it "returns true if the vector can be shifted" $ do
            canShift v2 `shouldBe` True

    describe "empty" $ do
        prop "returns n empty tiles" $ \ n ->
            -- TODO: There has to be a better way to set bounds.
            (n > 127) || (empty n == replicate n T.empty)

    describe "emptyIndexes" $ do
        it "returns the empty indexes" $ do
            emptyIndexes v1 `shouldBe` [0 .. length v1 - 1]

        it "doesn't return indexes with tiles" $ do
            emptyIndexes v2 `shouldBe` []

    describe "parse" $ do
        it "parses \"- - - -\"" $ do
            parse "- - - -" `shouldBe` v1

        prop "parses \"n n n n\"" $ \ n ->
            parse (unwords (replicate 4 (show n))) == replicate 4 (Just n)

    describe "render" $ do
        it "renders [Nothing, Nothing, Nothing, Nothing]" $ do
            render v1 `shouldBe` "- - - -"

        prop "renders [Just n, Just n, Just n, Just n]" $ \ n ->
            render (replicate 4 (Just n)) == unwords (replicate 4 (show n))

    describe "score" $ do
        it "scores [Nothing, Nothing, Nothing, Nothing]" $ do
            score v1 `shouldBe` 0

        prop "scores [Just n, Just n, Just n, Just n]" $ \ n ->
            score (replicate 4 (Just n)) == 4 * T.score (Just n)

    describe "set" $ do
        it "sets the tile at the index" $ do
            set v1 (Just 2) 0 `shouldBe` [Just 2, Nothing, Nothing, Nothing]

    describe "shift" $ do
        it "shifts the vector" $ do
            shift v2 `shouldBe` [Just 4, Just 4, Nothing, Nothing]
