module Core.Game(
         GameState(..),
         GameView(..),
        
         viewGS
        )
where

import Core
import qualified Data.Map as Map
import Data.List

data GameState = GameState {
    blocksGS :: [Block],
    gridSizeGS :: (Int,Int),
    currentPieceGS :: Piece,
    nextPieceGS :: Piece,
    kindsGS :: [PieceKind]
} 

instance Show GameState where
    show (GameState bs (a,b) _ np _) = concat . intersperse "\n" $[genLine (b-i) | i <- [0..(b-1)]]
        where
            grid = Map.fromList $ map ((\p -> (p,"#")).posBlock) bs
            genLine i = concat . intersperse " " $ [Map.findWithDefault "." (x,i) grid | x <- [0..(a-1)]] 

viewGS :: GameState -> GameView
viewGS (GameState bs size p np _) = GameView bs size (current p) (current np)

data GameView = GameView {
    blocksGV :: [Block],
    gridSizeGV :: (Int,Int),
    currentGV :: [Block],
    nextGV :: [Block]
}

