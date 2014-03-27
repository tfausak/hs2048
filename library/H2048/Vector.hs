-- | TODO
module H2048.Vector
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
import           Data.Maybe  (isJust)
import           Data.Maybe  (isNothing)
import           Data.Monoid ((<>))
import qualified H2048.Tile  as T

-- | TODO
type Vector = [T.Tile]

-- | TODO
canShift :: Vector -> Bool
canShift v = shift v /= v

-- | TODO
empty :: Int -> Vector
empty = flip replicate T.empty

-- | TODO
emptyIndexes :: Vector -> [Int]
emptyIndexes = fmap fst . filter (isNothing . snd) . zip [0 ..]

-- | TODO
parse :: String -> Vector
parse = fmap T.parse . words

-- | TODO
render :: Vector -> String
render = unwords . fmap T.render

-- | TODO
score :: Vector -> Int
score = sum . fmap T.score

-- | TODO
set :: Vector -> T.Tile -> Int -> Vector
set v t i = zipWith go [0 ..] v
  where
    go i' t' = if i' == i then t else t'

-- | TODO
shift :: Vector -> Vector
shift v = take n (v' <> empty n)
  where
    n = length v
    v' = group (filter isJust v) >>= go
    go (Just a : Just b : ts) = Just (a + b) : go ts
    go ts = ts
