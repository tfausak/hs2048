module Main (main) where

import           H2048         (new, play)
import           System.IO     as IO
import           System.Random (getStdGen)

main :: IO ()
main = do
    IO.hSetBuffering IO.stdin IO.NoBuffering

    r <- getStdGen
    play (new r)
