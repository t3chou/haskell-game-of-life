{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

import Lens.Micro ((^.), (%~))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=), (%=))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical), EventM
  , BrickEvent(..) 
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Main
  ( App(..), neverShowCursor, defaultMain
  , suspendAndResume, halt, getVtyHandle, showFirstCursor
  )
import Brick.Widgets.Core
import qualified Data.Array as A
import Life (GridState(..))

data Name = Button1 | Button2 | Button3 | 
            Button4 | Button5 | Button6 |
            Button7 | Button8 | Button9 |
            Cell { _idx :: L.GridIndex , _cst :: L.CellState}
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _state :: L.GridState
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
  [vBox [ 
          B.hBorderWithLabel (str "haskell game of life")
         , vBox [ buttonLayer st
             <+> B.vBorder
             <+> C.vCenter (padTopBottom 1 $ padLeftRight 5 $ gridLayer st L.gridRows L.gridCols) -- (hBox [ padLeftRight 5 $ str ("Current: \n" <> L.visualize (_state st) L.gridRows L.gridCols <> "\n")])
                  , str "(Press Esc to quit or n for the next state)" ]]]

gridLayer :: St -> Int -> Int -> Widget Name
gridLayer st rs cs = vBox (map (\r -> hBox (map (\c -> mc (r, c) (_state st)) [0..cs-1])) [0..rs-1])
    where
        mkCell cell = 
            clickable cell $
            --    padTopBottom 1 $
            --    padLeftRight (if wasClicked then 2 else 3) $
            str (if _cst cell == L.Alive then "o" else ".")
        mc idx (GridState g)  = mkCell (Cell idx (g A.! idx)) --str (case g A.! idx of
            --L.Alive -> "o"
            --L.Dead  -> ".") 
        -- cellChar L.Alive = "o"
        -- cellChar L.Dead  = "."
    

buttonLayer :: St -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (Pad 1) $ str "Select a Preset Grid:") <=>
      C.hCenterLayer (hBox $ padLeftRight 1 . padTopBottom 1 <$> buttons1) <=>
      C.hCenterLayer (hBox $ padLeftRight 1 . padTopBottom 1 <$> buttons2) <=>
      C.hCenterLayer (hBox $ padLeftRight 1 . padTopBottom 1 <$> buttons3)
    where
        buttons1 = mkButton <$> button1Data
        buttons2 = mkButton <$> button2Data
        buttons3 = mkButton <$> button3Data
        button1Data = [ (Button1, " Empty ", attrName "button1")
                      , (Button2, " Block ", attrName "button2")
                      , (Button3, "Beehive", attrName "button3")
                      ]
        button2Data = [ (Button4, " Loaf  ", attrName "button4")
                      , (Button5, " Boat  ", attrName "button5")
                      , (Button6, "  Tub  ", attrName "button6")
                      ]
        button3Data = [ (Button7, "Blinker", attrName "button7")
                      , (Button8, " Toad  ", attrName "button8")
                      , (Button9, " Beacon", attrName "button9")
                      ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^.lastReportedClick) == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
               padTopBottom 1 $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = do
    lastReportedClick .= Just (n, loc)
    case n of
        Button1 -> state %= const P.deadGrid
        Button2 -> state %= const (P.strToGrid P.block)
        Button3 -> state %= const (P.strToGrid P.beehive)
        Button4 -> state %= const (P.strToGrid P.loaf)
        Button5 -> state %= const (P.strToGrid P.boat)
        Button6 -> state %= const (P.strToGrid P.tub)
        Button7 -> state %= const (P.strToGrid P.blinker)
        Button8 -> state %= const (P.strToGrid P.toad)
        Button9 -> state %= const (P.strToGrid P.beacon)
        Cell idx cst -> state %= (\s -> L.toggleState s idx)
appEvent (T.MouseUp {}) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvMouseUp {})) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvKey (V.KChar 'n') [])) =
    state %= L.evolution
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    halt
appEvent ev =
    return ()

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "button1",   V.brightWhite `on` V.cyan)
    , (attrName "button2",   V.brightWhite `on` V.green)
    , (attrName "button3",   V.brightWhite `on` V.blue)
    , (attrName "button4",   V.brightWhite `on` V.red)
    , (attrName "button5",   V.brightWhite `on` V.yellow)
    , (attrName "button6",   V.brightWhite `on` V.magenta)
    , (attrName "button7",   V.brightWhite `on` V.brightMagenta)
    , (attrName "button8",   V.brightWhite `on` V.brightGreen)
    , (attrName "button9",   V.brightWhite `on` V.brightRed)
    ]

app :: App St e Name
app =
    App { appDraw = drawUi
          , appStartEvent = do
              vty <- getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , appHandleEvent = appEvent
          , appAttrMap = const aMap
          , appChooseCursor = showFirstCursor
          }

main :: IO ()
main = do
    void $ defaultMain app $ St [] Nothing (P.strToGrid P.beacon)


-- -- main :: IO ()
-- -- main = do 
-- --     let initialGrid = P.strToGrid P.beehive
-- --     loop initialGrid

-- -- loop :: L.GridState -> IO ()
-- -- loop currentSt = do
-- --     let newSt = L.evolution currentSt
-- --     L.visualize newSt L.gridRows L.gridCols
-- --     putStrLn ""
-- --     threadDelay 1000000
-- --     loop newSt
    

-- -- import qualified Life       as L
-- -- import qualified PresetGrid as P
-- -- import Control.Concurrent (threadDelay)

-- -- main :: IO ()
-- -- main = do 
-- --     L.visualize (P.strToGrid P.boat) L.gridRows L.gridCols
-- --     let initialGrid = P.strToGrid P.beacon
-- --     loop initialGrid

-- -- loop :: L.GridState -> IO ()
-- -- loop currentSt = do
-- --     let newSt = L.evolution currentSt
-- --     L.visualize newSt L.gridRows L.gridCols
-- --     putStrLn ""
-- --     L.visualize (L.evolution (P.strToGrid P.boat)) L.gridRows L.gridCols
-- --     threadDelay 1000000
-- --     loop newSt
