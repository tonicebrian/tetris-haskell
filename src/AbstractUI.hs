module AbstractUI(
        -- The View
        AbstractUI(AUI,processId),

        view,
        left,
        right,
        tick,
        dropPiece,
        rotateCW,
) where

import Core
import Game

import Processes
import Control.Distributed.Process
import Control.Distributed.Process.Closure

data AbstractUI = AUI {
    processId :: ProcessId
}

-- Static view. Refactor later. TODO
view :: AbstractUI -> IO GameView
view ui = undefined

left :: AbstractUI -> IO()
left ui@(AUI pid) = undefined

right :: AbstractUI -> IO()
right ui@(AUI pid) = undefined

tick :: AbstractUI -> IO()
tick ui@(AUI pid) = undefined

dropPiece :: AbstractUI -> IO()
dropPiece ui@(AUI pid) = undefined

rotateCW :: AbstractUI -> IO()
rotateCW ui@(AUI pid) = undefined

