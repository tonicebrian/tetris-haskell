{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Processes where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Core.Game
import Stage
import Data.Typeable (Typeable)
import Data.Binary
import Data.Data

data StageMessage = 
      MoveLeft
    | MoveRight
    | RotateCW
    | Tick
    | Drop
  deriving (Typeable)

-- By using newtypes we get the Binary instances for free
newtype View = View ProcessId deriving (Typeable, Binary)
newtype RemoteGameState = RGS GameState deriving (Typeable, Binary)

-- Let's use a simple codification of the possible orders
instance Binary StageMessage where
    put MoveLeft = put (0::Word8)
    put MoveRight = put (1::Word8)
    put RotateCW = put (2::Word8)
    put Tick = put (3::Word8)
    put Drop = put (4::Word8)

    get = do t <- get :: Get Word8
             case t of
                0 -> return MoveLeft
                1 -> return MoveRight
                2 -> return RotateCW
                3 -> return Tick
                4 -> return Drop

stageProcess :: GameState -> Process ()
stageProcess gs = do
    receiveWait [
        match $ \ move -> stageProcess (processMove move gs)
      , match $ \ (View pid) -> send pid (RGS gs) >> stageProcess gs
      ]
    where
        processMove MoveLeft = moveLeft
        processMove MoveRight = moveRight
        processMove RotateCW = rotateCW
        processMove Tick = tick
        processMove Drop = dropPiece

remotable ['stageProcess]
