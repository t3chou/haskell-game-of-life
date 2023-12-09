module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Life (evolution, GridState(..), CellState(..), gridRows, gridCols)
import PresetGrid (strToGrid, deadGrid)

import UI (runUI)
import PresetGrid (strToGrid, beacon)


main :: IO ()
main = runUI (strToGrid beacon)

loop :: L.GridState -> IO ()
loop currentSt = do
    let newSt = L.evolution currentSt
    L.visualize newSt L.gridRows L.gridCols
    putStrLn ""
    threadDelay 1000000
    loop newSt
    
