module Life
    (
        gridRows,
        gridCols,
        GridState(..),
        CellState(..),
        evolution,
        visualize
    ) where

import qualified Data.Array as A
import qualified Data.List  as L


-- | GridSize
gridRows :: Int
gridRows = 10

gridCols :: Int
gridCols = 10


-- | GridIndex (index into the state of the grid)
type GridIndex = (Int, Int)


-- | GridState (holds the current state of the grid)
newtype GridState = GridState (A.Array GridIndex CellState)


-- | CellState (Cells can be alive or dead)
data CellState = Alive | Dead
    deriving Eq


-- | Evolution (transform the state of the grid)
-- 1. Birth: A dead cell with exactly three live neighbors becomes a live cell,
--    as if by reproduction.
-- 2. Death by Isolation: A living cell with fewer than two live neighbors dies,
--    as if by loneliness.
-- 3. Death by Overcrowding: A living cell with more than three live neighbors
--    dies, as if by overcrowding.
-- 4. Survival: A living cell with two or three live neighbors continues to live,
--    unchanged, to the next generation.
evolution :: GridState -> GridState
evolution (GridState g) = GridState (g A.// updates)
    where
        dirs = [(1, 0), (-1, 0), (0, 1), (0, -1), (-1, 1), (1, 1), (-1, -1), (1, -1)]
        getState idx = if A.inRange (A.bounds g) idx
                            then Just (g A.! idx)
                            else Nothing
        liveNeigborsCnt idx = L.foldr (\aug acc -> acc + case getState aug of
                                        Nothing       -> 0
                                        Just    Dead  -> 0
                                        Just    Alive -> 1
                                       ) 0 [(fst idx + i, snd idx + j) | (i, j) <- dirs]
        nextState idx = case (g A.! idx, liveNeigborsCnt idx) of
                            (Dead,  3) -> Alive
                            (Alive, 4) -> Dead
                            (Alive, 1) -> Dead
                            (Alive, 0) -> Dead
                            (st,    _) -> st
        updates = [(idx, nextState idx) | idx <- A.indices g]


-- | Takes a GridState, grid rows, and grid cols as inputs and displays the grid
-- Use this for testing
visualize :: GridState -> Int -> Int -> IO()
visualize (GridState g) rs cs = do
    mapM_ (\r -> do
            mapM_  (\c -> do
                    let cell = g A.! (r, c)
                    putStr (cellChar cell)
                ) [0..cs-1]
            putStrLn ""
        ) [0..rs-1]
    where
        cellChar Alive = "o"
        cellChar Dead  = "."
