-- | Functions for pretty-printing games to the console.
module Hs2048.Renderer
    ( center
    , color
    , renderBoard
    , renderGame
    , renderTile
    ) where

import           Data.Monoid  ((<>))
import qualified Hs2048.Board as B
import qualified Hs2048.Tile  as T

{- |
    Centers a string in the given number of characters, using spaces for
    padding.

    >>> center 3 "x"
    " x "
    >>> center 2 "x"
    " x"
-}
center :: Int -> String -> String
center w s = prefix <> s <> suffix
  where
    prefix = replicate (ceiling n) ' '
    suffix = replicate (floor n) ' '
    n = fromIntegral (w - length s) / (2 :: Float)

{- |
    Calculates the color code for a tile. See
    <http://en.wikipedia.org/wiki/ANSI_escape_code#Colors>.

    >>> color Nothing
    30
    >>> color (Just 2)
    31
    >>> color (Just 2048)
    45
-}
color :: T.Tile -> Int
color t = 30 + x
  where
    x = if r > 6 then r + 4 else r
    r = T.rank t

{- |
    Renders a board with colorized, centered tiles. See 'renderTile' and
    'center'.

    >>> renderBoard [[Nothing, Just 2]]
    "\ESC[30m-\ESC[0m \ESC[31m2\ESC[0m\n"
    >>> renderBoard [[Nothing, Just 16]]
    " \ESC[30m-\ESC[0m \ESC[34m16\ESC[0m\n"
-}
renderBoard :: B.Board -> String
renderBoard b = unlines (fmap unwords vs')
  where
    vs' = fmap (fmap (center l)) vs
    vs = fmap (fmap renderTile) b
    l = maximum ls
    ls = vs >>= fmap length

{- |
    Renders a board along with its score. See 'renderBoard'.

    >>> renderGame [[Nothing, Just 2]]
    "Score: 0\n\ESC[30m-\ESC[0m \ESC[31m2\ESC[0m\n"
-}
renderGame :: B.Board -> String
renderGame b = concat
    [ "Score: "
    , show (B.score b)
    , "\n"
    , renderBoard b
    ]

{- |
    Renders a colorized tile. See 'color'.

    >>> renderTile Nothing
    "\ESC[30m-\ESC[0m"
    >>> renderTile (Just 2)
    "\ESC[31m2\ESC[0m"
-}
renderTile :: T.Tile -> String
renderTile t = concat
    [ "\ESC["
    , show (color t)
    , "m"
    , T.render t
    , "\ESC[0m"
    ]
