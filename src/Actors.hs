{-# LANGUAGE DeriveDataTypeable #-}
module Actors where

import Control.Distributed.Process

data StageMessage = 
      MoveLeft
    | MoveRight
    | RotateCW
    | Tick
    | Drop
    | View

stageActor :: Process ()
stageActor = undefined
