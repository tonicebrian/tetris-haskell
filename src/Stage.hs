module Stage(
        Stage,
        moveLeft,
        moveRight,
        rotateCW,
        tick,

        -- Constructor
        mkStage
        )
where

import Core 
import Data.List
import Data.Maybe

data Stage = Stage {
    size :: (Int,Int),
    currentPiece :: Piece,
    blocks :: [Block]
}

mkStage :: (Int,Int) -> Stage
mkStage s@(a,b) = Stage s cp bs
    where 
      dPos = (fromIntegral a/2.0,fromIntegral b-3)
      cp = mkPiece dPos TKind
      bs = (Block (0,0) TKind) : (current cp)

rotateCW :: GameState -> GameState
rotateCW = transit id $ flip rotateBy (-pi/2.0)

moveLeft :: GameState -> GameState
moveLeft = transit id $ flip moveBy (-1.0,0.0)

moveRight :: GameState -> GameState
moveRight = transit id $ flip moveBy (1.0,0.0)

tick :: GameState -> GameState
tick = transit spawn $ flip moveBy (0.0, -1.0)

spawn :: GameState -> GameState
spawn gs@(GameState bs (a,b) cp) = 
    let p = mkPiece (dropOffPos a b) TKind
    in GameState (bs++(current p)) (a,b) p

transit :: (GameState -> GameState) -> (Piece -> Piece) -> (GameState -> GameState)
transit onFail trans = \gs@(GameState bs (a,b) cp) -> 
    let unloaded = unload cp bs
        moved = trans cp
        newBlocks = load moved unloaded
        currentPoss = map posBlock $ current moved
    in if and [all (inBounds gs) currentPoss,
               (map posBlock unloaded `intersect` currentPoss) == [] ]
       then gs { blocksGS = newBlocks, currentPieceGS = moved }
       else onFail gs


inBounds :: GameState -> (Int,Int) -> Bool
inBounds gs (x,y) = (x >= 0) && (x <= a) && (y >= 0) && (y <= b)
    where
        (a,b) = gridSizeGS gs 

unload :: Piece -> [Block] -> [Block]
unload p bs = let currentPoss = map posBlock (current p)
              in filter (\x -> notElem (posBlock x) currentPoss) bs

load :: Piece -> [Block] -> [Block]
load p bs = bs ++ (current p) 
