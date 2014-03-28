-- | TODO
module H2048.Main
    ( getMove
    , play
    ) where

import           Data.Monoid     ((<>))
import qualified H2048.Board     as B
import qualified H2048.Direction as D
import qualified H2048.Game      as G
import qualified System.Random   as R

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
                Nothing -> putStrLn "Invalid move."
                Just d -> play (G.addRandomTile (B.move b d) r)
