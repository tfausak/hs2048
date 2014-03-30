-- | Types and functions for manipulating boards.
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

{- |
    Represents the game board. By convention, it is row-major.
-}
type Board = [V.Vector]

{- |
    Determines if the board can be moved in the given direction. See 'move'.

    >>> canMove [[Nothing, Just 2]] D.West
    True
-}
canMove :: Board -> D.Direction -> Bool
canMove b d = move b d /= b

{- |
    Determines if the board can be shifted. See 'shift'.

    >>> canShift [[Nothing, Just 2]]
    True
-}
canShift :: Board -> Bool
canShift b = shift b /= b

{- |
    Returns an empty board of the given size.

    >>> empty 2 1
    [[Nothing,Nothing]]
-}
empty :: Int -> Int -> Board
empty = flip replicate . V.empty

{- |
    Returns the points that don't contain tiles.

    >>> emptyPoints [[Nothing, Just 2]]
    [(0,0)]
-}
emptyPoints :: Board -> [P.Point]
emptyPoints b = zip [0 ..] (fmap V.emptyIndexes b) >>= go
  where
    go (x, ys) = fmap ((,) x) ys

{- |
    Moves the board in the given direction.

    >>> move [[Nothing, Just 2]] D.West
    [[Just 2,Nothing]]
-}
move :: Board -> D.Direction -> Board
move b d = rotateFrom (shift (rotateTo b d)) d

{- |
    Parses a string as a board. This is the inverse of 'render'.

    >>> parse "- 2\n4 -\n"
    [[Nothing,Just 2],[Just 4,Nothing]]
-}
parse :: String -> Board
parse = fmap V.parse . lines

{- |
    Renders a board as a string. This is the inverse of 'parse'.

    >>> render [[Nothing, Just 2], [Just 4, Nothing]]
    "- 2\n4 -\n"
-}
render :: Board -> String
render = unlines . fmap V.render

{- |
    Rotate the board 90 degrees clockwise.

    >>> rotate [[Nothing, Just 2], [Just 4, Nothing]]
    [[Just 4,Nothing],[Nothing,Just 2]]
-}
rotate :: Board -> Board
rotate = fmap reverse . transpose

{- |
    Rotates the board so that 'West' is at the left, assuming the given
    direction is currently at the left. This is the inverse of 'rotateTo'.

    >>> rotateFrom [[Just 4, Nothing], [Nothing, Just 2]] D.South
    [[Nothing,Just 2],[Just 4,Nothing]]
-}
rotateFrom :: Board -> D.Direction -> Board
rotateFrom b d = head (drop n gs)
  where
    n = 1 + fromEnum (maxBound :: D.Direction) - fromEnum d
    gs = iterate rotate b

{- |
    Rotates the board so that the given direction is at the left. This is the
    inverse of 'rotateFrom'

    >>> rotateTo [[Nothing, Just 2], [Just 4, Nothing]] D.South
    [[Just 4,Nothing],[Nothing,Just 2]]
-}
rotateTo :: Board -> D.Direction -> Board
rotateTo b d = head (drop n gs)
  where
    n = fromEnum d
    gs = iterate rotate b

{- |
    Calculates the score of a board.

    >>> score [[Nothing, Just 2], [Just 4, Just 8]]
    20
-}
score :: Board -> Int
score = sum . fmap V.score

{- |
    Sets a tile at the given point in the board.

    >>> set [[Nothing, Just 2], [Just 4, Nothing]] (Just 8) (1, 1)
    [[Nothing,Just 2],[Just 4,Just 8]]
-}
set :: Board -> T.Tile -> P.Point -> Board
set b t p = zipWith go [0 ..] b
  where
    go i v = if i == P.x p then V.set v t (P.y p) else v

{- |
    Shifts a board toward the head. See 'Vector.shift'.

    >>> shift [[Nothing, Just 2], [Just 4, Nothing]]
    [[Just 2,Nothing],[Just 4,Nothing]]
-}
shift :: Board -> Board
shift = fmap V.shift
