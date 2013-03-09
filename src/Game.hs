{-# LANGUAGE DeriveDataTypeable #-}
module Game(
         GameState(..),
         GameView(..),
         GameStatus(..),
        
         viewGS
        )
where

import Core
import qualified Data.Map as Map
import Data.List
import Data.Typeable (Typeable)
import Data.Binary
import System.Random

data GameState = GameState {
    blocksGS :: [Block],
    gridSizeGS :: (Int,Int),
    currentPieceGS :: Piece,
    nextPieceGS :: Piece,
    kindsGS :: [PieceKind],
    statusGS :: GameStatus
} deriving (Typeable)

instance Binary GameState where
    put (GameState bs size cp np ks st) = do
        put bs
        put size
        put cp
        put np
        put ks
        put st

    get = do bs <- get
             size <- get
             cp <- get
             np <- get
             ks <- get
             st <- get
             return (GameState bs size cp np ks st)
            
data GameStatus = Active
                | GameOver
                deriving (Show,Eq)

instance Binary GameStatus where
    put Active = put (0::Word8)
    put GameOver = put (1::Word8)

    get = do
        t <- get :: Get Word8
        case t of
            0 -> return Active
            1 -> return GameOver

instance Show GameState where
    show (GameState bs (a,b) _ np _ _) = concat . intersperse "\n" $[genLine (b-i) | i <- [0..(b-1)]]
        where
            grid = Map.fromList $ map ((\p -> (p,"#")).posBlock) bs
            genLine i = concat . intersperse " " $ [Map.findWithDefault "." (x,i) grid | x <- [0..(a-1)]] 

viewGS :: GameState -> GameView
viewGS (GameState bs size p np _ status) = GameView bs size (current p) (current np) status

data GameView = GameView {
    blocksGV :: [Block],
    gridSizeGV :: (Int,Int),
    currentGV :: [Block],
    nextGV :: [Block],
    statusGV :: GameStatus
}

