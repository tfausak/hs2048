-- | Types and functions for manipulating vectors.
module Hs2048.Vector
    ( Vector
    , canShift
    , empty
    , emptyIndexes
    , parse
    , render
    , score
    , set
    , shift
    ) where

import           Data.List   (group)
import           Data.Maybe  (isJust, isNothing)
import           Data.Monoid ((<>))
import qualified Hs2048.Tile as T

{- |
    Represents a row or column on the game board. By convention, a vector has
    4 tiles.
-}
type Vector = [T.Tile]

{- |
    Determines if the vector can be shifted.

    >>> canShift [Nothing, Just 2, Nothing, Nothing]
    True
-}
canShift :: Vector -> Bool
canShift v = shift v /= v

{- |
    Returns an empty vector of the given size.

    >>> empty 4
    [Nothing,Nothing,Nothing,Nothing]
-}
empty :: Int -> Vector
empty = flip replicate T.empty

{- |
    Returns the indexes that don't contain tiles.

    >>> emptyIndexes [Nothing, Just 2, Nothing, Nothing]
    [0,2,3]
-}
emptyIndexes :: Vector -> [Int]
emptyIndexes = fmap fst . filter (isNothing . snd) . zip [0 ..]

{- |
    Parses a string as a vector. This is the inverse of 'render'.

    >>> parse "- 2 - -"
    [Nothing,Just 2,Nothing,Nothing]
-}
parse :: String -> Vector
parse = fmap T.parse . words

{- |
    Renders a vector as a string. This is the inverse of 'parse'.

    >>> render [Nothing, Just 2, Nothing, Nothing]
    "- 2 - -"
-}
render :: Vector -> String
render = unwords . fmap T.render

{- |
    Calculates the score of a vector.

    >>> score [Nothing, Just 2, Just 4, Just 8]
    20
-}
score :: Vector -> Int
score = sum . fmap T.score

{- |
    Sets a tile at the given index in the vector.

    >>> set [Nothing, Nothing, Nothing, Nothing] (Just 2) 1
    [Nothing,Just 2,Nothing,Nothing]
-}
set :: Vector -> T.Tile -> Int -> Vector
set v t i = zipWith go [0 ..] v
  where
    go i' t' = if i' == i then t else t'

{- |
    Shifts a vector toward the head. The output vector will be the same size as
    the input vector, padded with @Nothing@.

    >>> shift [Nothing, Just 2, Nothing, Nothing]
    [Just 2,Nothing,Nothing,Nothing]

    Like tiles will be combined.

    >>> shift [Just 2, Nothing, Just 2, Just 2]
    [Just 4,Just 2,Nothing,Nothing]

    Any number of tiles can be combined in one shift.

    >>> shift [Just 2, Just 2, Just 4, Just 4]
    [Just 4,Just 8,Nothing,Nothing]
-}
shift :: Vector -> Vector
shift v = take n (v' <> empty n)
  where
    n = length v
    v' = group (filter isJust v) >>= go
    go (Just a : Just b : ts) = Just (a + b) : go ts
    go ts = ts
