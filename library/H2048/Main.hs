-- | TODO
module H2048.Main
    ( getMove
    ) where

import qualified H2048.Direction as D

-- | TODO
getMove :: IO (Maybe D.Direction)
getMove = do
    a <- getChar
    if a /= '\ESC' then return Nothing else do
        b <- getChar
        if b /= '[' then return Nothing else do
            c <- getChar
            return $ case c of
                'D' -> Just D.West
                'B' -> Just D.South
                'C' -> Just D.East
                'A' -> Just D.North
                _ -> Nothing
