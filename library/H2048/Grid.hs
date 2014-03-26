-- | TODO
module H2048.Grid
    ( Grid
    , empty
    , move
    , parse
    , render
    , rotate
    , rotateFrom
    , rotateTo
    , score
    , shift
    ) where

import           Data.List       (transpose)
import qualified H2048.Direction as D
import qualified H2048.Vector    as V

-- | TODO
type Grid = [V.Vector]

-- | TODO
empty :: Int -> Int -> Grid
empty = flip replicate . V.empty

-- | TODO
move :: Grid -> D.Direction -> Grid
move g d = rotateFrom (shift (rotateTo g d)) d

-- | TODO
parse :: String -> Grid
parse = fmap V.parse . lines

-- | TODO
render :: Grid -> String
render = unlines . fmap V.render

-- | TODO
rotate :: Grid -> Grid
rotate = fmap reverse . transpose

-- | TODO
rotateFrom :: Grid -> D.Direction -> Grid
rotateFrom g d = head (drop n gs)
  where
    n = 1 + fromEnum (maxBound :: D.Direction) - fromEnum d
    gs = iterate rotate g

-- | TODO
rotateTo :: Grid -> D.Direction -> Grid
rotateTo g d = head (drop n gs)
  where
    n = fromEnum d
    gs = iterate rotate g

-- | TODO
score :: Grid -> Int
score = sum . fmap V.score

-- | TODO
shift :: Grid -> Grid
shift = fmap V.shift
