module Main (main) where

import           H2048         (new, play)
import           System.Random (getStdGen)

main :: IO ()
main = getStdGen >>= play . new
