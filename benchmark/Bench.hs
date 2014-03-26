module Main (main) where

import           Criterion.Main    (bgroup, defaultMain)
import qualified H2048.GridBench
import qualified H2048.TileBench
import qualified H2048.VectorBench
import qualified H2048Bench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = defaultMain
    [ bgroup "H2048" H2048Bench.benchmarks
    , bgroup "H2048.Tile" H2048.TileBench.benchmarks
    , bgroup "H2048.Vector" H2048.VectorBench.benchmarks
    , bgroup "H2048.Grid" H2048.GridBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
