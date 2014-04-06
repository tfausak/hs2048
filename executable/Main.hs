module Main (main) where

import           Hs2048             (new, play)
import           System.Environment (getArgs)
import           System.Random      (StdGen, getStdGen)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [seed] -> do
            let g = read seed
            play (new (g :: StdGen))
        _ -> do
            g <- getStdGen
            let seed = show g
            print seed
            play (new g)
