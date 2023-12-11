import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import qualified Data.Array as A
import Life (GridState(..), CellState(..), evolution, gridRows, gridCols, visualize, toggleState)
import PresetGrid (strToGrid, boat)

data AppState = AppState {
    gameGrid :: GridState,
    isPaused :: Bool
}

drawUI :: AppState -> [Widget n]
drawUI (AppState grid paused) = 
    [ center $ 
      borderWithLabel (str $ if paused then "Paused - Space to run, Enter to step, Esc to exit" else "Running - Space to pause") $
      str $ visualize grid gridRows gridCols
    ]

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state (VtyEvent (V.EvKey (V.KChar ' ') [])) = 
    continue $ state { isPaused = not (isPaused state) }
handleEvent state (VtyEvent (V.EvKey V.KEnter [])) | isPaused state = 
    continue $ state { gameGrid = evolution (gameGrid state) }
handleEvent state _ = continue state

app :: App AppState e n
app = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const theMap
}

theMap :: AttrMap
theMap = attrMap V.defAttr []


main :: IO ()
main = do
    let initialState = AppState { gameGrid = strToGrid boat, isPaused = True }
    finalState <- defaultMain app initialState
    putStrLn "Game Ended"

loop :: L.GridState -> IO ()
loop currentSt = do
    let newSt = L.evolution currentSt
    L.visualize newSt L.gridRows L.gridCols
    putStrLn ""
    L.visualize (L.evolution (P.strToGrid P.boat)) L.gridRows L.gridCols
    threadDelay 1000000
    loop newSt
