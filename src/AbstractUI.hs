module AbstractUI(
        -- The View
        AbstractUI(..),
        view,
        emptyUI
) where

import Core

data AbstractUI = AbstractUI

-- Static view. Refactor later. TODO
view :: AbstractUI -> GameView
view ui = GameView blocks' size' current'
    where
        blocks' = [Block (5,5) TKind, Block (6,5) TKind, Block (7,5) TKind, Block (6,6) TKind, Block (0,0) TKind]
        size'   = (10,20)
        current' = [Block (5,5) TKind, Block (6,5) TKind, Block (7,5) TKind, Block (6,6) TKind]

emptyUI = AbstractUI


