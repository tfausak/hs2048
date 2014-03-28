-- | TODO
module H2048.Main
    ( direction
    , getMove
    , play
    ) where

import           Data.Monoid     ((<>))
import qualified H2048.Board     as B
import qualified H2048.Direction as D
import qualified H2048.Game      as G
import qualified System.Random   as R

-- | TODO
direction :: Char -> Maybe D.Direction
direction 'D' = Just D.West
direction 'B' = Just D.South
direction 'C' = Just D.East
direction 'A' = Just D.North
direction _ = Nothing

-- | TODO
getMove :: IO (Maybe D.Direction)
getMove = do
    a <- getChar
    if a /= '\ESC' then return Nothing else do
        b <- getChar
        if b /= '[' then return Nothing else do
            c <- getChar
            return (direction c)

-- | TODO
play :: R.RandomGen r => (B.Board, r) -> IO ()
play (b, r) = do
    putStrLn ("Score: " <> show (B.score b))
    putStr (B.render b)
    if G.isOver b
        then putStrLn "Game over."
        else do
            m <- getMove
            case m of
                Nothing -> do
                    putStrLn "Unknown move."
                    play (b, r)
                Just d -> do
                    if B.canMove b d
                        then play (G.addRandomTile (B.move b d) r)
                        else do
                            putStrLn "Invalid move."
                            play (b, r)
