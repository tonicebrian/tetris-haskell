module Stage(
        Stage,
        moveLeft,
        moveRight,
        rotateCW,
        tick,
        view,

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

view :: Stage -> GameView
view stage = GameView (blocks stage) (size stage) (current (currentPiece stage))

rotateCW :: Stage -> Stage
rotateCW stage = transformPiece stage $ flip rotateBy (-pi/2.0)

moveLeft :: Stage -> Stage
moveLeft stage = transformPiece stage $ flip moveBy (-1.0,0.0)

moveRight :: Stage -> Stage
moveRight stage = transformPiece stage $ flip moveBy (1.0,0.0)

tick :: GameState -> GameState
tick s = undefined

transformPiece :: Stage -> (Piece -> Piece) -> Stage
transformPiece s@(Stage (a,b) cp bs) trans =
    let unloaded = unload cp bs
        moved = trans cp
        newBlocks = load moved unloaded
    in if all (inBounds s) $ map posBlock (current moved) 
       then s {currentPiece = moved, blocks = newBlocks}
       else s

inBounds :: Stage -> (Int,Int) -> Bool
inBounds s (x,y) = (x >= 0) && (x <= a) && (y >= 0) && (y <= b)
    where
        (a,b) = size s 

unload :: Piece -> [Block] -> [Block]
unload p bs = let currentPoss = map posBlock (current p)
              in filter (\x -> notElem (posBlock x) currentPoss) bs

load :: Piece -> [Block] -> [Block]
load p bs = bs ++ (current p) 
