-- | TODO
module H2048.Main
    ( getMove
    ) where

import qualified H2048.Direction as D

-- | TODO
getMove :: IO (Maybe D.Direction)
getMove = do
    a <- getChar
    b <- getChar
    c <- getChar

    return $ case [a, b, c] of
        "\ESC[D" -> Just D.West
        "\ESC[B" -> Just D.South
        "\ESC[C" -> Just D.East
        "\ESC[A" -> Just D.North
        _ -> Nothing
