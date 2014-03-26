-- | TODO
module H2048.Grid
    ( Grid
    , empty
    , parse
    , render
    , score
    , shift
    ) where

import qualified H2048.Vector as V

-- | TODO
type Grid = [V.Vector]

-- | TODO
empty :: Int -> Int -> Grid
empty = flip replicate . V.empty

-- | TODO
parse :: String -> Grid
parse = fmap V.parse . lines

-- | TODO
render :: Grid -> String
render = unlines . fmap V.render

-- | TODO
score :: Grid -> Int
score = sum . fmap V.score

-- | TODO
shift :: Grid -> Grid
shift = fmap V.shift
