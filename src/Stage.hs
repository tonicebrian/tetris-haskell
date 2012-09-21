module Stage(
        Stage,
        moveLeft,
        moveRight,
        view,

        -- Constructor
        mkStage
        )
where

import Core hiding (blocks)

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

moveLeft :: Stage -> Stage
moveLeft stage = stageMoveBy stage (-1.0) 0.0

moveRight :: Stage -> Stage
moveRight stage = stageMoveBy stage 1.0 0.0

stageMoveBy :: Stage -> Double -> Double -> Stage
stageMoveBy s@(Stage (a,b) cp bs) x y = 
    let unloaded = unload cp bs
        moved = moveBy cp (x,y)
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
