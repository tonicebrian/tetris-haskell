module Core (
        PieceKind(..),
        Block(..),
        GameView(..)
        )
where

data PieceKind = IKind
                | JKind
                | LKind
                | OKind
                | SKind
                | TKind
                | ZKind

data Block = Block {
    pos :: (Int,Int),
    kind :: PieceKind
}

data GameView = GameView {
    blocks :: [Block],
    gridSize :: (Int,Int),
    current :: [Block]
}
