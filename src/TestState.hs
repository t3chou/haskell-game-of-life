import qualified Life       as L
import qualified PresetGrid as P
import qualified Data.Array.IArray as A


-- >>> block2 == tester P.block
-- >>> beehive2 == tester P.beehive
-- >>> loaf2 == tester P.loaf
-- >>> boat2 == tester P.boat
-- >>> tub2 == tester P.tub
-- >>> blinker2 == tester P.blinker 
-- >>> toad2 == tester P.toad 
-- >>> beacon2 == tester P.beacon
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
--

-- >>> tester P.beacon
-- ".................................oo........o............o........oo................................."


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

