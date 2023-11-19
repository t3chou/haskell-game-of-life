module PresetGrid
    (
        deadGrid,
        strToGrid,
        block,
        beehive,
        loaf,
        boat,
        tub,
        blinker,
        toad,
        beacon
    ) where

import qualified Data.Array.IArray as A
import qualified Life              as L


-- | deadGrid starts with all cells in the dead state
deadGrid :: L.GridState
deadGrid = L.GridState (
            A.array
            ((0, 0), (L.gridRows-1, L.gridCols-1))
            [((i, j), L.Dead) | i <- [0..L.gridRows-1], j <- [0..L.gridCols-1]]
        )


-- | strToGrid converts a string to a GridState
strToGrid :: String -> L.GridState
strToGrid s = L.GridState
            (A.array
            ((0, 0), (L.gridRows-1, L.gridCols-1))
            (zip [(i, j) | i <- [0..L.gridRows-1], j <- [0..L.gridCols-1]] (map toInt s))
        )
        where
            toInt '.' = L.Dead
            toInt 'o' = L.Alive
            toInt _   = undefined


-- | Still Life
block :: String
block = "\
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

beehive :: String
beehive = "\
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

loaf :: String
loaf = "\
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

boat :: String
boat = "\
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

tub :: String
tub = "\
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
blinker :: String
blinker = "\
    \..........\
    \..........\
    \..........\
    \..........\
    \....o.....\
    \....o.....\
    \....o.....\
    \..........\
    \..........\
    \.........."

toad :: String
toad = "\
    \..........\
    \..........\
    \..........\
    \.....o....\
    \...o..o...\
    \...o..o...\
    \....o.....\
    \..........\
    \..........\
    \.........."

beacon :: String
beacon = "\
    \..........\
    \..........\
    \..........\
    \...oo.....\
    \...oo.....\
    \.....oo...\
    \.....oo...\
    \..........\
    \..........\
    \.........."
