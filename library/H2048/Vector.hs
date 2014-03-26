-- | TODO
module H2048.Vector
    ( Vector
    , empty
    , parse
    , render
    , score
    ) where

import qualified H2048.Tile as T

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
