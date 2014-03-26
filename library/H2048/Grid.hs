-- | TODO
module H2048.Grid
    ( Grid
    , parse
    , render
    , score
    ) where

import qualified H2048.Vector as V

-- | TODO
type Grid = [V.Vector]

-- | TODO
parse :: String -> Grid
parse = fmap V.parse . lines

-- | TODO
render :: Grid -> String
render = unlines . fmap V.render

-- | TODO
score :: Grid -> Int
score = sum . fmap V.score
