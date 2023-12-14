{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Life       as L
import qualified PresetGrid as P
import Control.Concurrent (threadDelay)

import Lens.Micro ((^.), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V
import Lens.Micro.Mtl (use, (.=), (%=))

import Brick.Main
  ( App(..), neverShowCursor, defaultMain
  , suspendAndResume, halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , EventM
  , BrickEvent(..) 
  )
import Brick.Widgets.Core
  ( vBox
  , str
  )

data St =
    St { _state :: L.GridState
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [ui]
    where
        ui = vBox [ str $ "Current state: \n" <> L.visualize (_state st) L.gridRows L.gridCols <> "\n"
                  , str "(Press Esc to quit or n for the next state)"
                  ]

appEvent :: BrickEvent () e -> EventM () St ()
appEvent (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt
        V.EvKey (V.KChar 'n') [] -> do
            state %= L.evolution
        _ -> return ()
appEvent _ = return ()

initialState :: St
initialState = 
    St { _state = P.strToGrid P.beacon
       }

theApp :: App St e ()
theApp = 
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main =
    void $ defaultMain theApp initialState

    
-- main :: IO ()
-- main = do 
--     let initialGrid = P.strToGrid P.beehive
--     loop initialGrid

-- loop :: L.GridState -> IO ()
-- loop currentSt = do
--     let newSt = L.evolution currentSt
--     L.visualize newSt L.gridRows L.gridCols
--     putStrLn ""
--     threadDelay 1000000
--     loop newSt
    

-- import qualified Life       as L
-- import qualified PresetGrid as P
-- import Control.Concurrent (threadDelay)

-- main :: IO ()
-- main = do 
--     L.visualize (P.strToGrid P.boat) L.gridRows L.gridCols
--     let initialGrid = P.strToGrid P.beacon
--     loop initialGrid

-- loop :: L.GridState -> IO ()
-- loop currentSt = do
--     let newSt = L.evolution currentSt
--     L.visualize newSt L.gridRows L.gridCols
--     putStrLn ""
--     L.visualize (L.evolution (P.strToGrid P.boat)) L.gridRows L.gridCols
--     threadDelay 1000000
--     loop newSt
