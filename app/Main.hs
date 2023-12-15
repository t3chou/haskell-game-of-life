{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

import Lens.Micro ((^.), (%~), set)
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
  , suspendAndResume, halt, getVtyHandle, showFirstCursor, vScrollBy, viewportScroll
  )
import Brick.Widgets.Core

data Name = Block | Beehive | Loaf | Boat | Tub | Blinker | Toad | Beacon | Reset | Next | Quit | TextBox | Load
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _state :: L.GridState
       , _step :: Int 
       , _edit :: E.Editor String Name
       , _msg :: String
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
  [vBox [ 
          B.hBorderWithLabel (str "haskell game of life")
         , vBox [ buttonLayer st
             <+> B.vBorder
             <+> C.vCenter (hBox [ padLeftRight 5 $ str ("Current: \n"<> (_msg st) <> L.visualize (_state st) L.gridRows L.gridCols <> "\n")])
                  , str "(Press Esc to quit or n for the next state)" ]]]

buttonLayer :: St -> Widget Name
buttonLayer st =
    vBox [(padBottom (Pad 1) $ str ("Current steps:" <> show (_step st))) , B.hBorder,
    (padBottom (Pad 1) $ str "Select profile (always live):") 
    ,(hBox $ padAll 1 <$> buttons), 
    vBox [ vBox[(padBottom (Pad 1) $ str "Select profile (oscillators):") 
    ,(hBox $ padAll 1 <$> buttons1), (padBottom (Pad 1) $ str "Controls:") 
    ,(hBox $ padAll 1 <$> buttons2)] <+> B.vBorder
             <+> vBox[(padAll 1 $ strWrap "Or enter text and then click in this editor:") ,
       (padLeft (Pad 1) $ vLimit 12 $ hLimit 15 $ E.renderEditor (str . unlines) True (st^.edit))]]]
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Block, "Block", attrName "Block")
                     , (Beehive, "Beehive", attrName "Beehive")
                     , (Loaf, "Loaf", attrName "Loaf")
                     , (Boat, "Boat", attrName "Boat")
                     , (Tub, "Tub", attrName "Tub")
                     ]
        buttons1 = mkButton <$> buttonData1
        buttonData1 = [ (Blinker, "Blinker", attrName "Blinker")
                     , (Toad, "Toad", attrName "Toad")
                     , (Beacon, "Beacon", attrName "Beacon")
                     ]
        buttons2 = mkButton <$> buttonData2
        buttonData2 = [ (Reset, "Reset", attrName "Reset")
                     , (Next, "Next", attrName "Next")
                     , (Quit, "Quit", attrName "Quit")
                     , (Load, "Load", attrName "Load")
                     ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^.lastReportedClick) == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
            --    padTopBottom 1 $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = do
        lastReportedClick .= Just (n, loc)
        T.zoom edit $ E.handleEditorEvent ev
        case n of
            Block -> do 
                state .= (P.strToGrid P.block)
                step .= 0
            Beehive -> do 
                state .= (P.strToGrid P.beehive)
                step .= 0
            Loaf -> do 
                state .= (P.strToGrid P.loaf)
                step .= 0
            Boat -> do 
                state .= (P.strToGrid P.boat)
                step .= 0
            Tub -> do 
                state .= (P.strToGrid P.tub)
                step .= 0
            Blinker -> do 
                state .= (P.strToGrid P.blinker)
                step .= 0
            Toad -> do 
                state .= (P.strToGrid P.toad)
                step .= 0
            Beacon -> do 
                state .= (P.strToGrid P.beacon)
                step .= 0
            Reset -> do 
                state .= P.deadGrid
                step .= 0
            Next -> do 
                state %= L.evolution
                step %= (+ 1)
            Quit -> do 
                halt
            TextBox -> return ()
            Load -> do
                editor <- use edit
                let contents = E.getEditContents editor
                state .= (P.strToGrid (concat (lines (concat contents))))
                step .= 0
            
appEvent (T.VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    state %= L.evolution
    step %= (+ 1)
appEvent (T.MouseUp {}) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvMouseUp {})) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    halt
appEvent ev = 
    T.zoom edit $ E.handleEditorEvent ev

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "Block",   V.white `on` V.green)
    , (attrName "Beehive",   V.white `on` V.green)
    , (attrName "Loaf",   V.white `on` V.green)
    , (attrName "Boat",   V.white `on` V.green)
    , (attrName "Tub",   V.white `on` V.green)
    , (attrName "Blinker",   V.white `on` V.blue)
    , (attrName "Toad",   V.white `on` V.blue)
    , (attrName "Beacon",   V.white `on` V.blue)
    , (attrName "Reset",   V.white `on` V.cyan)
    , (attrName "Next",   V.white `on` V.cyan)
    , (attrName "Load",   V.white `on` V.cyan)
    , (attrName "Quit",   V.white `on` V.cyan)
    , (E.editFocusedAttr, V.black `on` V.yellow)
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
    void $ defaultMain app $ St [] Nothing P.deadGrid 0 (E.editor TextBox Nothing (unlines (split P.blinker))) ""

split :: String -> [String]
split "" = []
split s = take 10 s : split (drop 10 s)

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
