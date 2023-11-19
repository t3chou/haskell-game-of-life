module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P

main :: IO ()
main = do 
    L.visualize (P.strToGrid P.boat) L.gridRows L.gridCols
    putStrLn ""
    L.visualize (L.evolution (P.strToGrid P.boat)) L.gridRows L.gridCols
    