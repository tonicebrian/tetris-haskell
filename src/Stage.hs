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

--view :: Stage -> GameView
--view stage = GameView (blocks stage) (size stage) (current (currentPiece stage))

rotateCW :: GameState -> GameState
rotateCW = transit $ flip rotateBy (-pi/2.0)

moveLeft :: GameState -> GameState
moveLeft = transit $ flip moveBy (-1.0,0.0)

moveRight :: GameState -> GameState
moveRight = transit $ flip moveBy (1.0,0.0)

tick :: GameState -> GameState
tick = undefined

transit :: (Piece -> Piece) -> (GameState -> GameState)
transit trans = \gs@(GameState bs (a,b) cp) -> 
    let unloaded = unload cp bs
        moved = trans cp
        newBlocks = load moved unloaded
    in if all (inBounds gs) $ map posBlock (current moved) 
       then gs {currentPieceGS = moved, blocksGS = newBlocks}
       else gs


inBounds :: GameState -> (Int,Int) -> Bool
inBounds gs (x,y) = (x >= 0) && (x <= a) && (y >= 0) && (y <= b)
    where
        (a,b) = gridSizeGS gs 

unload :: Piece -> [Block] -> [Block]
unload p bs = let currentPoss = map posBlock (current p)
              in filter (\x -> notElem (posBlock x) currentPoss) bs

load :: Piece -> [Block] -> [Block]
load p bs = bs ++ (current p) 
