{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Processes where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Node (initRemoteTable)
import Network.Transport.TCP
import Control.Distributed.Process.Closure
import Game
import Core
import Stage
import Data.Typeable (Typeable)
import Data.Binary
import Data.Data
import System.Random

data StageMessage = 
      MoveLeft
    | MoveRight
    | RotateCW
    | Tick
    | Drop
  deriving (Typeable)

-- By using newtypes we get the Binary instances for free
newtype RemoteView = RemoteView ProcessId deriving (Typeable, Binary)
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

initializeGame :: StdGen -> Process ()
initializeGame seed = stageProcess state
    where
        state = Stage.mkState [Block (0,0) TKind] kinds
        kinds = map (pieces !!) (randomRs (0,(length pieces)-1) seed) 
        pieces = [IKind,JKind,LKind,OKind,SKind,TKind,ZKind]

stageProcess :: GameState -> Process ()
stageProcess gs = receiveWait [
        match $ \ (RemoteView pid) -> do
            liftIO $ putStrLn "Recibido un View en el actor remoto"
            send pid (RGS gs)
            liftIO $ putStrLn $ "Enviado el RGS al master " ++ show pid
            stageProcess gs
        , match $ \ move -> do
            say "Recibido un move"
            stageProcess (processMove move gs)
      ]
    where
        processMove MoveLeft = moveLeft
        processMove MoveRight = moveRight
        processMove RotateCW = rotateCW
        processMove Tick = tick
        processMove Drop = dropPiece

remotable ['stageProcess]
