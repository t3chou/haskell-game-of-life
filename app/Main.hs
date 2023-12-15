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

data Name = Button1 | Button2 | Button3
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
             <+> C.vCenter (hBox [ padLeftRight 5 $ str ("Current: \n" <> L.visualize (_state st) L.gridRows L.gridCols <> "\n")])
                  , str "(Press Esc to quit or n for the next state)" ]]]

buttonLayer :: St -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (Pad 1) $ str "Click a button:") <=>
      C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons) 
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Button1, "Button 1", attrName "button1")
                     , (Button2, "Button 2", attrName "button2")
                     , (Button3, "Button 3", attrName "button3")
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
    [ (attrName "button1",   V.white `on` V.cyan)
    , (attrName "button2",   V.white `on` V.green)
    , (attrName "button3",   V.white `on` V.blue)
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
