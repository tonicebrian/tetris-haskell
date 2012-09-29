module Core.Game(
         GameState(..),
         GameView(..),
        
         viewGS
        )
where

import Core

data GameState = GameState {
    blocksGS :: [Block],
    gridSizeGS :: (Int,Int),
    currentPieceGS :: Piece,
    nextPieceGS :: Piece,
    kindsGS :: [PieceKind]
} deriving Show

viewGS :: GameState -> GameView
viewGS (GameState bs size p _ _) = GameView bs size (current p)

data GameView = GameView {
    blocksGV :: [Block],
    gridSizeGV :: (Int,Int),
    currentGV :: [Block]
}

