module Main (main) where

import qualified PresetGrid as P
import qualified Life as L

main :: IO ()
main = L.visualize P.deadGrid L.gridRows L.gridCols
