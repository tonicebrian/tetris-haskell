module AbstractUI(
        -- The View
        AbstractUI(AUI,processId),

        view,
        left,
        right,
        tick,
        dropPiece,
        rotateCW,
        -- Constructors
        mkUI
) where

import Core
import Core.Game
import qualified Stage as S
import System.Random

import Processes
import Control.Distributed.Process
import Control.Distributed.Process.Closure

data AbstractUI = AUI {
    processId :: ProcessId
}

-- Static view. Refactor later. TODO
view :: AbstractUI -> Process GameView
view ui = undefined

mkUI :: StdGen -> Process ()
mkUI seed = stageProcess state
    where
        state = S.mkState [Block (0,0) TKind] kinds
        kinds = map (pieces !!) (randomRs (0,(length pieces)-1) seed) 
        pieces = [IKind,JKind,LKind,OKind,SKind,TKind,ZKind]

left :: AbstractUI -> Process()
left ui@(AUI pid) = send pid MoveLeft

right :: AbstractUI -> Process()
right ui@(AUI pid) = undefined

tick :: AbstractUI -> Process()
tick ui@(AUI pid) = undefined

dropPiece :: AbstractUI -> Process()
dropPiece ui@(AUI pid) = undefined

rotateCW :: AbstractUI -> Process()
rotateCW ui@(AUI pid) = undefined

