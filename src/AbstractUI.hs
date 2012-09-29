module AbstractUI(
        -- The View
        AbstractUI,

        view,
        left,
        right,
        tick,
        -- Constructors
        newUI
) where

import Core
import Core.Game
import qualified Stage as S
import System.Random

data AbstractUI = AUI {
    state :: GameState
}

-- Static view. Refactor later. TODO
view :: AbstractUI -> GameView
view ui = viewGS (state ui)

newUI seed = AUI (S.mkState [Block (0,0) TKind] kinds) 
    where
        kinds = map (pieces !!) (randomRs (0,(length pieces)-1) seed) 
        pieces = [IKind,JKind,LKind,OKind,SKind,TKind,ZKind]

left :: AbstractUI -> AbstractUI
left ui = let old = state ui
              new = S.moveLeft old
          in ui { state = new }

right :: AbstractUI -> AbstractUI
right ui = let old = state ui
               new = S.moveRight old
           in ui { state = new }

tick :: AbstractUI -> AbstractUI
tick ui@(AUI old) = ui { state = S.tick old }


