module Core (
        PieceKind(..),
        Block(..),
        Piece(posPiece),

        -- Constructors
        mkPiece,
        current,

        -- Functions
        moveBy,
        rotateBy,
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

dropOffPos :: Int -> Int -> (Double,Double)
dropOffPos x y = (fromIntegral x/2.0,fromIntegral y-3.0)

data Piece = Piece {
    posPiece :: (Double,Double),
    kindPiece :: PieceKind,
    locals :: [(Double,Double)]
} deriving Show

mkPiece :: (Double,Double) -> PieceKind -> Piece 
mkPiece pos TKind = Piece pos TKind [(-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (0.0, 1.0)] 
mkPiece pos IKind = Piece pos IKind [(-1.5, 0.0), (-0.5, 0.0), (0.5, 0.0), (1.5, 0.0)]
mkPiece pos JKind = Piece pos JKind [(-1.0, 0.5), (0.0, 0.5), (1.0, 0.5), (1.0, -0.5)]
mkPiece pos LKind = Piece pos LKind [(-1.0, 0.5), (0.0, 0.5), (1.0, 0.5), (-1.0, -0.5)]
mkPiece pos OKind = Piece pos OKind [(-0.5, 0.5), (0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)]
mkPiece pos SKind = Piece pos SKind [(0.0, 0.5), (1.0, 0.5), (-1.0, -0.5), (0.0, -0.5)]
mkPiece pos ZKind = Piece pos ZKind [(-1.0, 0.5), (0.0, 0.5), (0.0, -0.5), (1.0, -0.5)]

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
