module Life
    (
        gridRows,
        gridCols,
        GridState(..),
        CellState(..),
        visualize
    ) where

import qualified Data.Array as A


-- | GridSize
gridRows :: Int
gridRows = 100

gridCols :: Int
gridCols = 100


-- | GridIndex (index into the state of the grid)
type GridIndex = (Int, Int)


-- | GridState (holds the current state of the grid)
newtype GridState = GridState
    {
        grid :: A.Array GridIndex CellState
    }


-- | CellState (Cells can be alive or dead)
data CellState = Alive | Dead
    deriving Eq


-- | Evolution (a transformer to transform the state of a given cell)
-- 1. Birth: A dead cell with exactly three live neighbors becomes a live cell,
--    as if by reproduction.
-- 2. Death by Isolation: A living cell with fewer than two live neighbors dies,
--    as if by loneliness.
-- 3. Death by Overcrowding: A living cell with more than three live neighbors
--    dies, as if by overcrowding.
-- 4. Survival: A living cell with two or three live neighbors continues to live,
--    unchanged, to the next generation.
evolution :: a
evolution = undefined


-- | Takes a GridState, grid rows, and grid cols as inputs and displays the grid
-- Use this for testing
visualize :: GridState -> Int -> Int -> IO()
visualize g rs cs = do
    mapM_ (\r -> do
            mapM_  (\c -> do
                    let cell = grid g A.! (r, c)
                    putStr (cellChar cell)
                ) [0..cs-1]
            putStrLn ""
        ) [0..rs-1]
    where
        cellChar Alive = "o"
        cellChar Dead = "."
