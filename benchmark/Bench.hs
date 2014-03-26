module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified H2048Bench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = defaultMain
    [ bgroup "H2048" H2048Bench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
