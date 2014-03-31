module Main (main) where

import           Criterion.Main       (bgroup, defaultMain)
import qualified H2048.BoardBench
import qualified H2048.DirectionBench
import qualified H2048.GameBench
import qualified H2048.MainBench
import qualified H2048.PointBench
import qualified H2048.RendererBench
import qualified H2048.SettingsBench
import qualified H2048.TileBench
import qualified H2048.VectorBench
import qualified H2048Bench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = defaultMain
    [ bgroup "H2048" H2048Bench.benchmarks
    , bgroup "H2048.Tile" H2048.TileBench.benchmarks
    , bgroup "H2048.Vector" H2048.VectorBench.benchmarks
    , bgroup "H2048.Board" H2048.BoardBench.benchmarks
    , bgroup "H2048.Direction" H2048.DirectionBench.benchmarks
    , bgroup "H2048.Point" H2048.PointBench.benchmarks
    , bgroup "H2048.Game" H2048.GameBench.benchmarks
    , bgroup "H2048.Settings" H2048.SettingsBench.benchmarks
    , bgroup "H2048.Main" H2048.MainBench.benchmarks
    , bgroup "H2048.Renderer" H2048.RendererBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
