module Main (main) where

import           Criterion.Main        (bgroup, defaultMain)
import qualified Hs2048.AIBench
import qualified Hs2048.BoardBench
import qualified Hs2048.DirectionBench
import qualified Hs2048.GameBench
import qualified Hs2048.MainBench
import qualified Hs2048.PointBench
import qualified Hs2048.RendererBench
import qualified Hs2048.SettingsBench
import qualified Hs2048.TileBench
import qualified Hs2048.VectorBench
import qualified Hs2048Bench

main :: IO ()
main = defaultMain
    [ bgroup "Hs2048" Hs2048Bench.benchmarks
    , bgroup "Hs2048.AI" Hs2048.AIBench.benchmarks
    , bgroup "Hs2048.Board" Hs2048.BoardBench.benchmarks
    , bgroup "Hs2048.Direction" Hs2048.DirectionBench.benchmarks
    , bgroup "Hs2048.Game" Hs2048.GameBench.benchmarks
    , bgroup "Hs2048.Main" Hs2048.MainBench.benchmarks
    , bgroup "Hs2048.Point" Hs2048.PointBench.benchmarks
    , bgroup "Hs2048.Renderer" Hs2048.RendererBench.benchmarks
    , bgroup "Hs2048.Settings" Hs2048.SettingsBench.benchmarks
    , bgroup "Hs2048.Tile" Hs2048.TileBench.benchmarks
    , bgroup "Hs2048.Vector" Hs2048.VectorBench.benchmarks
    ]
