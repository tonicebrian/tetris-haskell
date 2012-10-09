{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
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
    | View
  deriving (Typeable)

instance Binary StageMessage where
    put MoveLeft = put (0::Word8)
    put MoveRight = put (1::Word8)
    put RotateCW = put (2::Word8)
    put Tick = put (3::Word8)
    put Drop = put (4::Word8)
    put View = put (5::Word8)

    get = do t <- get :: Get Word8
             case t of
                0 -> return MoveLeft
                1 -> return MoveRight
                2 -> return RotateCW
                3 -> return Tick
                4 -> return Drop
                5 -> return View

stageProcess :: GameState -> Process ()
stageProcess gs = do
    ngs <- receiveWait [match processMove]
    stageProcess ngs
  where
    processMove MoveLeft = return $ moveLeft gs
    processMove _ = undefined
    
remotable ['stageProcess]
