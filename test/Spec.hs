-- QuickCheck Test for evolution

module Spec where

import Test.QuickCheck
import Life
import PresetGrid
import qualified Data.Array as A

-- Property to test if a dead cell with exactly three live neighbors becomes alive
prop_birthRule :: GridState -> GridIndex -> Bool
prop_birthRule initialState idx =
    let neighbors = liveNeighbors initialState idx
        cellState = currentState initialState idx
    in case cellState of
        Dead -> neighbors == 3
        _    -> True  -- If the cell is not dead, the birth rule does not apply

-- Property to test survival rule
prop_survivalRule :: GridState -> GridIndex -> Bool
prop_survivalRule initialState idx =
    let neighbors = liveNeighbors initialState idx
        cellState = currentState initialState idx
    in case cellState of
        Alive -> neighbors == 2 || neighbors == 3
        _     -> True  -- If the cell is not alive, the survival rule does not apply

-- Property to test grid initialization
prop_gridInitialization :: Bool
prop_gridInitialization =
    let grid = initializeGrid examplePreset
    in A.bounds grid == ((0,0), (gridHeight - 1, gridWidth - 1))

-- Test if the evolution function preserves the grid size
prop_evolutionPreservesGridSize :: GridState -> Bool
prop_evolutionPreservesGridSize grid =
    let evolvedGrid = evolve grid
    in A.bounds grid == A.bounds evolvedGrid

-- Test for edge cases
prop_edgeCases :: GridState -> Bool
prop_edgeCases grid = undefined  -- Implementation for edge cases

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests
