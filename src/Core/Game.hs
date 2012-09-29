module Core.Game(
         GameState(..),
         GameView(..),
        
         mkState,
         viewGS
        )
 where

import Core

data GameState = GameState {
    blocksGS :: [Block],
    gridSizeGS :: (Int,Int),
    currentPieceGS :: Piece
} deriving Show

mkState :: [Block] -> GameState
mkState bs = let (x,y) = (10,20) :: (Int,Int)
                 p = mkPiece (dropOffPos x y) TKind
            in GameState (bs++(current p)) (x,y) p

viewGS :: GameState -> GameView
viewGS (GameState bs size p) = GameView bs size (current p)

data GameView = GameView {
    blocksGV :: [Block],
    gridSizeGV :: (Int,Int),
    currentGV :: [Block]
}

