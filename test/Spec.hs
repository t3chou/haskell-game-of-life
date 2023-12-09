module Main where

import Test.QuickCheck
import qualified Data.Array as A
import Life (evolution, GridState(..), CellState(..), gridRows, gridCols)
import PresetGrid (strToGrid, deadGrid)

-- Define Arbitrary instances for the data types
instance Arbitrary CellState where
    arbitrary = elements [Alive, Dead]

instance Arbitrary GridState where
    arbitrary = do
        cells <- vectorOf (gridRows * gridCols) arbitrary
        let bounds = ((0, 0), (gridRows - 1, gridCols - 1))
        return $ GridState (A.array bounds cells)

-- Property: The grid size remain constant after each evolution
prop_GridSizeConstant :: GridState -> Bool
prop_GridSizeConstant (GridState grid) =
    let GridState newGrid = evolution (GridState grid)
    in  A.bounds grid == A.bounds newGrid

-- Property: A dead grid stays dead after evolution
prop_DeadGridStaysDead :: Bool
prop_DeadGridStaysDead =
    let GridState initialGrid = deadGrid
        GridState evolvedGrid = evolution initialGrid
    in  all (== Dead) (A.elems evolvedGrid)

-- Main function to run tests
main :: IO ()
main = do
    quickCheck prop_GridSizeConstant
    quickCheck prop_DeadGridStaysDead
