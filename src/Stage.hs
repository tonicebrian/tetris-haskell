module Stage(
        moveLeft,
        moveRight,
        rotateCW,
        dropPiece,
        tick,
        mkState
        )
where

import Core 
import Core.Game
import Data.List
import Data.Maybe

mkState :: [Block] -> [PieceKind] -> GameState
mkState bs kinds =
     let (x,y) = (10,20) :: (Int,Int)
         p = mkPiece (0,0) TKind
         withNext = spawn(GameState [] (x,y) p p kinds Active)
     in spawn( withNext { blocksGS = bs } )

dropPiece :: GameState -> GameState
dropPiece s = (tick . ts) s
    where
        ts :: GameState -> GameState
        ts = foldr (.) id (take (snd $ gridSizeGS s) (repeat moveDown))
        moveDown = transit id $ flip moveBy (0.0,-1.0)

rotateCW :: GameState -> GameState
rotateCW = transit id $ flip rotateBy (-pi/2.0)

moveLeft :: GameState -> GameState
moveLeft = transit id $ flip moveBy (-1.0,0.0)

moveRight :: GameState -> GameState
moveRight = transit id $ flip moveBy (1.0,0.0)

tick :: GameState -> GameState
tick = transit (spawn . clearFullRow)  $ flip moveBy (0.0, -1.0)

spawn :: GameState -> GameState
spawn gs@(GameState bs (a,b) p np ks st) = 
    let next = mkPiece (2,1) (head ks)
        p = np { posPiece = (dropOffPos a b) }
        s1 = (GameState bs (a,b) p next (tail ks) st)
        good = validate s1 >>= \s -> Just s { blocksGS = (current p)++bs }
    in fromMaybe (s1 {blocksGS = (current p)++bs,statusGS = GameOver}) good

clearFullRow :: GameState -> GameState
clearFullRow gs@(GameState bs (a,b) cp _ _ _) = tryRow (b-1) gs
    where
        tryRow :: Int -> GameState -> GameState 
        tryRow i s@(GameState bs (a,b) cp _ _ _)
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
isFullRow i s@(GameState bs (a,b) cp _ _ _) =
    (length $ filter (\x -> (snd $ posBlock x) == i) bs) == a

transit :: (GameState -> GameState) -> (Piece -> Piece) -> (GameState -> GameState)
transit onFail trans = \gs@(GameState bs (a,b) cp _ _ st) -> 
    case st of
        Active -> 
            let unloaded = unload cp bs
                moved = trans cp
                newBlocks = load moved unloaded
                s1 = validate (gs {blocksGS=unloaded,currentPieceGS=moved}) >>= 
                     \s -> Just s { blocksGS = newBlocks }
            in fromMaybe (onFail gs) s1
        GameOver -> gs

validate :: GameState -> Maybe GameState
validate gs@(GameState bs size p np st _)=
    let currentPoss  = map posBlock $ current p
    in if and [all (inBounds size) currentPoss,
               (map posBlock bs `intersect` currentPoss) == [] ]
       then Just gs
       else Nothing
  where
    inBounds :: (Int,Int) -> (Int,Int) -> Bool
    inBounds (a,b) (x,y) = (x >= 0) && (x < a) && (y >= 0) && (y < b)

unload :: Piece -> [Block] -> [Block]
unload p bs = let currentPoss = map posBlock (current p)
              in filter (\x -> notElem (posBlock x) currentPoss) bs

load :: Piece -> [Block] -> [Block]
load p bs = bs ++ (current p) 
