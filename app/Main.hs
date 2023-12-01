module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)

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
    