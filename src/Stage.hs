module Stage(
        moveLeft,
        moveRight,
        rotateCW,
        tick,
        )
where

import Core 
import Core.Game
import Data.List
import Data.Maybe

rotateCW :: GameState -> GameState
rotateCW = transit id $ flip rotateBy (-pi/2.0)

moveLeft :: GameState -> GameState
moveLeft = transit id $ flip moveBy (-1.0,0.0)

moveRight :: GameState -> GameState
moveRight = transit id $ flip moveBy (1.0,0.0)

tick :: GameState -> GameState
tick = transit (spawn . clearFullRow)  $ flip moveBy (0.0, -1.0)

spawn :: GameState -> GameState
spawn gs@(GameState bs (a,b) cp) = 
    let p = mkPiece (dropOffPos a b) TKind
    in GameState (bs++(current p)) (a,b) p

clearFullRow :: GameState -> GameState
clearFullRow gs@(GameState bs (a,b) cp) = tryRow (b-1) gs
    where
        tryRow :: Int -> GameState -> GameState 
        tryRow i s@(GameState bs (a,b) cp)
            | i<0 = s
            | otherwise = 
                let blocksBelow = filter (\x -> (snd $ posBlock x) < i) bs
                    blocksAbove = filter (\x -> (snd $ posBlock x) > i) bs
                    moveBlocks1RowDown = \b@(Block (x,y) _) -> b { posBlock = (x,y-1) }
                in if isFullRow i s
                   then tryRow (i-1) (s {blocksGS = blocksBelow ++ 
                                                    map moveBlocks1RowDown blocksAbove })
                   else tryRow (i-1) s

isFullRow :: Int -> GameState -> Bool
isFullRow i s@(GameState bs (a,b) cp) =
    (length $ filter (\x -> (snd $ posBlock x) == i) bs) == a

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
