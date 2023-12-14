-- QuickCheck Test for Conway's Game of Life

module Spec where

import Test.QuickCheck
import Life
import PresetGrid
import qualified Data.Array as A

-- Helper function to count live neighbors
liveNeighbors :: GridState -> GridIndex -> Int
liveNeighbors (GridState grid) (x, y) = length $ filter isLive neighbors
  where
    neighbors = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], (i, j) /= (x, y)]
    isLive idx = case grid A.! idx of
                    Alive -> True
                    _     -> False

-- Property to test if a dead cell with exactly three live neighbors becomes alive
prop_birthRule :: GridState -> GridIndex -> Property
prop_birthRule initialState idx = 
    within 10000 $
    (grid A.! idx == Dead && liveNeighbors initialState idx == 3) ==>
    let newState = evolution initialState in
    (newState `getCellValue` idx) == Alive
  where
    grid = getGrid initialState

-- Property to test survival rule
prop_survivalRule :: GridState -> GridIndex -> Property
prop_survivalRule initialState idx = 
    within 10000 $
    (grid A.! idx == Alive && liveNeighbors initialState idx `elem` [2, 3]) ==>
    let newState = evolution initialState in
    (newState `getCellValue` idx) == Alive
  where
    grid = getGrid initialState

-- Property to test overpopulation and underpopulation rules
prop_deathRule :: GridState -> GridIndex -> Property
prop_deathRule initialState idx = 
    within 10000 $
    (grid A.! idx == Alive && (liveNeighbors initialState idx < 2 || liveNeighbors initialState idx > 3)) ==>
    let newState = evolution initialState in
    (newState `getCellValue` idx) == Dead
  where
    grid = getGrid initialState

-- Helper function to get the cell value from a GridState
getCellValue :: GridState -> GridIndex -> CellState
getCellValue (GridState grid) idx = grid A.! idx

-- Helper function to get the grid from a GridState
getGrid :: GridState -> A.Array GridIndex CellState
getGrid (GridState grid) = grid

-- Tests for preset grids
prop_presetGrids :: Property
prop_presetGrids = 
    within 10000 $
    conjoin [testPresetGrid block, testPresetGrid beehive, testPresetGrid loaf, 
             testPresetGrid boat, testPresetGrid tub, testPresetGrid blinker,
             testPresetGrid toad, testPresetGrid beacon]

testPresetGrid :: GridState -> Bool
testPresetGrid gridState = evolution gridState == gridState  -- Preset grids should remain unchanged after one evolution

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "Testing Conway's Game of Life"
  quickCheck prop_birthRule
  quickCheck prop_survivalRule
  quickCheck prop_deathRule
  quickCheck prop_presetGrids
