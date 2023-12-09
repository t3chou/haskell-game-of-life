module UI where

import Brick
import Brick.Main (App(..), showFirstCursor, defaultMain)
import Brick.Types (BrickEvent, EventM, Next, continue, halt)
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Life (evolution, GridState(..), CellState(..), gridRows, gridCols)
import qualified Data.Array as A

-- Define app state
data AppState = AppState {
    gridState :: GridState,
    isRunning :: Bool
}

-- Define cell attributes
cellAttr :: CellState -> AttrName
cellAttr Alive = attrName "cellAlive"
cellAttr Dead  = attrName "cellDead"

-- Convert GridState to a Brick widget
drawGrid :: GridState -> Widget n
drawGrid (GridState grid) =
    withBorderStyle unicode $
    borderWithLabel (str "Conway's Game of Life") $
    vBox rows
    where
        rows = [hBox $ cellsInRow y | y <- [0..gridRows-1]]
        cellsInRow y = [drawCell x y | x <- [0..gridCols-1]]
        drawCell x y = withAttr (cellAttr $ grid A.! (x, y)) $ str "  "

-- The main function for the Brick application
app :: App AppState e ()
app = App { 
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr [
        (cellAttr Alive, V.white `on` V.green),
        (cellAttr Dead, V.white `on` V.black)
    ]
}

-- Function to draw the UI
drawUI :: AppState -> [Widget ()]
drawUI state = [center $ drawGrid (gridState state)]

-- Event handling
handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent state (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> halt state
        -- other cases
        _ -> continue state
handleEvent state _ = continue state

-- Function to run the Brick application
runUI :: GridState -> IO ()
runUI grid = do
    let initialState = AppState grid False
    _ <- defaultMain app initialState
    return ()
