module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)
import qualified Graphics.Vty as V
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

main :: IO ()
main = do 
    let initialGrid = P.strToGrid P.beacon
    loop initialGrid

loop :: L.GridState -> IO ()
loop currentSt = do
    let newSt = L.evolution currentSt
    L.visualize newSt L.gridRows L.gridCols
    putStrLn ""
    threadDelay 1000000
    loop newSt
    
