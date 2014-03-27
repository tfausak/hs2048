-- | TODO
module H2048.Board
    ( Board
    , canMove
    , canShift
    , empty
    , emptyPoints
    , move
    , parse
    , render
    , rotate
    , rotateFrom
    , rotateTo
    , score
    , set
    , shift
    ) where

import           Data.List       (transpose)
import qualified H2048.Direction as D
import qualified H2048.Point     as P
import qualified H2048.Tile      as T
import qualified H2048.Vector    as V

-- | TODO
type Board = [V.Vector]

-- | TODO
canMove :: Board -> D.Direction -> Bool
canMove b d = move b d /= b

-- | TODO
canShift :: Board -> Bool
canShift b = shift b /= b

-- | TODO
empty :: Int -> Int -> Board
empty = flip replicate . V.empty

-- | TODO
emptyPoints :: Board -> [P.Point]
emptyPoints b = zip [0 ..] (fmap V.emptyIndexes b) >>= go
  where
    go (x, ys) = fmap ((,) x) ys

-- | TODO
move :: Board -> D.Direction -> Board
move b d = rotateFrom (shift (rotateTo b d)) d

-- | TODO
parse :: String -> Board
parse = fmap V.parse . lines

-- | TODO
render :: Board -> String
render = unlines . fmap V.render

-- | TODO
rotate :: Board -> Board
rotate = fmap reverse . transpose

-- | TODO
rotateFrom :: Board -> D.Direction -> Board
rotateFrom b d = head (drop n gs)
  where
    n = 1 + fromEnum (maxBound :: D.Direction) - fromEnum d
    gs = iterate rotate b

-- | TODO
rotateTo :: Board -> D.Direction -> Board
rotateTo b d = head (drop n gs)
  where
    n = fromEnum d
    gs = iterate rotate b

-- | TODO
score :: Board -> Int
score = sum . fmap V.score

-- | TODO
set :: Board -> T.Tile -> P.Point -> Board
set b t p = zipWith go [0 ..] b
  where
    go i v = if i == P.x p then V.set v t (P.y p) else v

-- | TODO
shift :: Board -> Board
shift = fmap V.shift
