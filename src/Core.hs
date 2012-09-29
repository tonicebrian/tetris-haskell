module Core (
        PieceKind(..),
        Block(..),
        GameView(..),
        GameState(..),
        Piece,

        -- Constructors
        mkPiece,
        current,
        mkState,

        -- Functions
        moveBy,
        rotateBy,
        viewGS,
        dropOffPos
        )
where

data PieceKind = IKind
                | JKind
                | LKind
                | OKind
                | SKind
                | TKind
                | ZKind
                deriving (Eq,Show)

data Block = Block {
    posBlock :: (Int,Int),
    kindBlock :: PieceKind
} deriving (Eq,Show)

data GameState = GameState {
    blocksGS :: [Block],
    gridSizeGS :: (Int,Int),
    currentPieceGS :: Piece
} deriving Show

dropOffPos :: Int -> Int -> (Double,Double)
dropOffPos x y = (fromIntegral x/2.0,fromIntegral y-3.0)

mkState :: [Block] -> GameState
mkState bs = let (x,y) = (10,20) :: (Int,Int)
                 p = mkPiece (dropOffPos x y) TKind
            in GameState (bs++(current p)) (x,y) p

viewGS :: GameState -> GameView
viewGS (GameState bs size p) = GameView bs size (current p)

data GameView = GameView {
    blocksGV :: [Block],
    gridSizeGV :: (Int,Int),
    currentGameView :: [Block]
}
data Piece = Piece {
    posPiece :: (Double,Double),
    kindPiece :: PieceKind,
    locals :: [(Double,Double)]
} deriving Show

mkPiece :: (Double,Double) -> PieceKind -> Piece 
mkPiece pos TKind = Piece pos TKind [(-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (0.0, 1.0)] 
mkPiece _ _ = error "No suitable kind"

current :: Piece -> [Block]
current (Piece (a,b) kind locals) = 
    map (\(x,y) -> Block (floor (x+a), floor (y+b)) kind) locals

moveBy :: Piece -> (Double,Double) -> Piece
moveBy p@(Piece (a,b) _ _) (x,y) = p {posPiece = (a+x,b+y)}

rotateBy :: Piece -> Double -> Piece
rotateBy p@(Piece (a,b) _ ls) theta = 
    let c = cos theta
        s = sin theta
        roundToHalf :: (Double,Double) -> (Double,Double)
        roundToHalf (a,b) = ((fromIntegral . round $ a*2.0) * 0.5, (fromIntegral .round $ b*2.0) * 0.5)
    in p {
        locals = map roundToHalf $ map (\(x,y) -> (x * c - y * s, x * s + y * c)) ls
       }
