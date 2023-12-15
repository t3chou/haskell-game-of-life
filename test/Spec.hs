-- QuickCheck Test for Conway's Game of Life
module Main where
-- module Spec where

import Test.QuickCheck
import qualified Life as L
import qualified PresetGrid as P 
import PresetGrid
import qualified Data.Array as A

instance Arbitrary L.CellState where
    arbitrary = elements [L.Alive, L.Dead]

instance Arbitrary L.GridState where
    arbitrary = do
        states <- vectorOf (L.gridRows * L.gridCols) arbitrary
        return $ L.GridState (A.listArray ((0, 0), (L.gridRows - 1, L.gridCols - 1)) states)

-- Helper function to count live neighbors
liveNeighbors :: L.GridState -> L.GridIndex -> Int
liveNeighbors (L.GridState grid) (x, y) = length $ filter isLive neighbors
  where
    neighbors = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], (i, j) /= (x, y)]
    isLive idx = case grid A.! idx of
                    L.Alive -> True
                    _     -> False

-- Property to test if a dead cell with exactly three live neighbors becomes alive
prop_birthRule :: L.GridState -> L.GridIndex -> Property
prop_birthRule initialState idx = 
    within 10000 $
    (grid A.! idx == L.Dead && liveNeighbors initialState idx == 3) ==>
    let newState = L.evolution initialState in
    (newState `getCellValue` idx) == L.Alive
  where
    grid = getGrid initialState

-- Property to test survival rule
prop_survivalRule :: L.GridState -> L.GridIndex -> Property
prop_survivalRule initialState idx = 
    within 10000 $
    (grid A.! idx == L.Alive && liveNeighbors initialState idx `elem` [2, 3]) ==>
    let newState = L.evolution initialState in
    (newState `getCellValue` idx) == L.Alive
  where
    grid = getGrid initialState

-- Property to test overpopulation and underpopulation rules
prop_deathRule :: L.GridState -> L.GridIndex -> Property
prop_deathRule initialState idx = 
    within 10000 $
    (grid A.! idx == L.Alive && (liveNeighbors initialState idx < 2 || liveNeighbors initialState idx > 3)) ==>
    let newState = L.evolution initialState in
    (newState `getCellValue` idx) == L.Dead
  where
    grid = getGrid initialState

-- Helper function to get the cell value from a L.GridState
getCellValue :: L.GridState -> L.GridIndex -> L.CellState
getCellValue (L.GridState grid) idx = grid A.! idx

-- Helper function to get the grid from a L.GridState
getGrid :: L.GridState -> A.Array L.GridIndex L.CellState
getGrid (L.GridState grid) = grid

-- Tests for preset grids
prop_presetGrids :: Property
prop_presetGrids = 
    within 10000 $
    conjoin [testPresetGrid (strToGrid block), testPresetGrid (strToGrid beehive), testPresetGrid (strToGrid loaf), 
             testPresetGrid (strToGrid boat), testPresetGrid (strToGrid tub), testPresetGrid (strToGrid blinker),
             testPresetGrid (strToGrid toad), testPresetGrid (strToGrid beacon)]


testPresetGrid :: L.GridState -> Bool
testPresetGrid (L.GridState grid) = L.evolution (L.GridState grid) == (L.GridState grid)



tester :: String -> String
tester grid = do 
    let initialGrid = P.strToGrid grid
        newState = L.evolution initialGrid
    gridToString newState L.gridRows L.gridCols

-- Use this for testing
gridToString :: L.GridState -> Int -> Int -> String
gridToString (L.GridState g) rs cs =
    let cellChar L.Alive = 'o'
        cellChar L.Dead  = '.'
        rowToString r = [cellChar (g A.! (r, c)) | c <- [0..cs-1]] -- ++ "\n"
    in concat [rowToString r | r <- [0..rs-1]]


-- Property: The number of cells in the grid remains constant after evolution.
prop_evolutionConservesCells :: L.GridState -> Bool
prop_evolutionConservesCells grid =
    let L.GridState arr = L.evolution grid
    in length (A.assocs arr) == L.gridRows * L.gridCols



-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "Testing Conway's Game of Life"
--   quickCheck prop_birthRule
--   quickCheck prop_survivalRule
  -- quickCheck prop_deathRule
  -- quickCheck prop_presetGrids
  quickCheck prop_evolutionConservesCells
  putStrLn $ if block2 == tester P.block then "OK" else "FAIL!"
  putStrLn $ if beehive2 == tester P.beehive then "OK" else "FAIL!"
  putStrLn $ if loaf2 == tester P.loaf then "OK" else "FAIL!"
  putStrLn $ if boat2 == tester P.boat then "OK" else "FAIL!"
  putStrLn $ if tub2 == tester P.tub then "OK" else "FAIL!"
  putStrLn $ if blinker2 == tester P.blinker  then "OK" else "FAIL!"
  putStrLn $ if toad2 == tester P.toad  then "OK" else "FAIL!"
  putStrLn $ if beacon2 == tester P.beacon then "OK" else "FAIL!"

-- | New State (Still Life)
block2 :: String
block2 = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \....oo....\
    \....oo....\
    \..........\
    \..........\
    \..........\
    \.........."

beehive2 :: String
beehive2 = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \....oo....\
    \...o..o...\
    \....oo....\
    \..........\
    \..........\
    \.........."

loaf2 :: String
loaf2 = "\
    \..........\
    \..........\
    \..........\
    \....oo....\
    \...o..o...\
    \....o.o...\
    \.....o....\
    \..........\
    \..........\
    \.........."

boat2 :: String
boat2 = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \...oo.....\
    \...o.o....\
    \....o.....\
    \..........\
    \..........\
    \.........."

tub2 :: String
tub2 = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \....o.....\
    \...o.o....\
    \....o.....\
    \..........\
    \..........\
    \.........."


-- | Oscillators
blinker2 :: String
blinker2 = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \..........\
    \...ooo....\
    \..........\
    \..........\
    \..........\
    \.........."

toad2 :: String
toad2 = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \....ooo...\
    \...ooo....\
    \..........\
    \..........\
    \..........\
    \.........."

beacon2 :: String
beacon2 = "\
    \..........\
    \..........\
    \..........\
    \...oo.....\
    \...o......\
    \......o...\
    \.....oo...\
    \..........\
    \..........\
    \.........."
