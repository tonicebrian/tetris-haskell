module AbstractUI(
        -- The View
        AbstractUI,

        view,
        left,
        right,
        -- Constructors
        newUI
) where

import Core
import qualified Stage as S

data AbstractUI = AUI {
    stage :: S.Stage
}

-- Static view. Refactor later. TODO
view :: AbstractUI -> GameView
view ui = S.view (stage ui)

newUI = AUI (S.mkStage (10,20)) 

left :: AbstractUI -> AbstractUI
left ui = let old = stage ui
              new = S.moveLeft old
          in ui { stage = new }

right :: AbstractUI -> AbstractUI
right ui = let old = stage ui
               new = S.moveRight old
           in ui { stage = new }


