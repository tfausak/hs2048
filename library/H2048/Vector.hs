-- | TODO
module H2048.Vector
    ( Vector
    , empty
    , parse
    , render
    , score
    , shift
    ) where

import           Data.List   (group)
import           Data.Maybe  (isJust)
import           Data.Monoid ((<>))
import qualified H2048.Tile  as T

-- | TODO
type Vector = [T.Tile]

-- | TODO
empty :: Int -> Vector
empty = flip replicate T.empty

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
shift :: Vector -> Vector
shift v = take n (v' <> empty n)
  where
    n = length v
    v' = group (filter isJust v) >>= go
    go (Just a : Just b : ts) = Just (a + b) : go ts
    go ts = ts
