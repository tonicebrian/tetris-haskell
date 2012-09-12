module Core (
        PieceKind(..),
        Block(..),
        GameView(..),
        Piece,

        -- Constructors
        mkPiece,
        current,

        -- Functions
        moveBy
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
