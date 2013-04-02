module AbstractUI(
        -- The View
        AbstractUI(..),
        GUICommands(..),

        view,
        left,
        right,
        tick,
        dropPiece,
        rotateCW,
) where

import Core
import Game

import Control.Concurrent as C

data GUICommands =
      MoveLeft
    | MoveRight
    | RotateCW
    | Tick
    | Drop
    | View

data AbstractUI = AbstractUI (C.Chan GUICommands) (C.Chan GameView)

-- Static view. Refactor later. TODO
view :: AbstractUI -> IO GameView
view (AbstractUI _ reply) = C.readChan reply

left :: AbstractUI -> IO()
left (AbstractUI req _) = C.writeChan req MoveLeft

right :: AbstractUI -> IO()
right (AbstractUI req _) = C.writeChan req MoveRight

tick :: AbstractUI -> IO()
tick (AbstractUI req _) = C.writeChan req Tick

dropPiece :: AbstractUI -> IO()
dropPiece (AbstractUI req _) = C.writeChan req Drop

rotateCW :: AbstractUI -> IO()
rotateCW (AbstractUI req _) = C.writeChan req RotateCW

