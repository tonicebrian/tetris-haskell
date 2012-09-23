module Core (
        PieceKind(..),
        Block(..),
        GameView(..),
        Piece,

        -- Constructors
        mkPiece,
        current,

        -- Functions
        moveBy,
        rotateBy
        )
where

data PieceKind = IKind
                | JKind
                | LKind
                | OKind
                | SKind
                | TKind
                | ZKind
                deriving Eq

data Block = Block {
    posBlock :: (Int,Int),
    kindBlock :: PieceKind
} deriving Eq

data GameView = GameView {
    blocks :: [Block],
    gridSize :: (Int,Int),
    currentGameView :: [Block]
}

data Piece = Piece {
    posPiece :: (Double,Double),
    kindPiece :: PieceKind,
    locals :: [(Double,Double)]
}

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
