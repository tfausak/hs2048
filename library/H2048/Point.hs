-- | TODO
module H2048.Point
    ( Point
    , x
    , y
    ) where

-- | TODO
type Point = (Int, Int)

-- | TODO
x :: Point -> Int
x = fst

-- | TODO
y :: Point -> Int
y = snd
