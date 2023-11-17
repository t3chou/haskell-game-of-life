module PresetGrid
    (
        deadGrid
    ) where

import qualified Data.Array as A
import qualified Life as L


-- | deadGrid starts with all cells in the dead state
deadGrid :: L.GridState
deadGrid = L.GridState
    { 
        L.grid = A.array 
            ((0, 0), (L.gridRows-1, L.gridCols-1)) 
            [((i, j), L.Dead) | i <- [0..L.gridRows-1], j <- [0..L.gridCols-1]]
    }
