module Main(Main.main) where


import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Node (initRemoteTable)
import Network.Transport.TCP
import Control.Concurrent as C
import Control.Monad

import System.Random

import qualified Processes as P
import AbstractUI
import GUI
import AbstractUI
import Game

masterProc :: AbstractUI -> ProcessId -> Process () 
masterProc (AbstractUI requests replies) slaveId = do
    masterId <- getSelfPid
    forever $ do
        cmd <- liftIO $ C.readChan requests
        case cmd of
            MoveLeft  -> send slaveId P.MoveLeft
            MoveRight -> send slaveId P.MoveRight
            RotateCW  -> send slaveId P.RotateCW
            Tick      -> send slaveId P.Tick
            Drop      -> send slaveId P.Drop
            View      -> do
                send slaveId $ P.View masterId
                (P.RGS gs) <- expect :: Process P.RemoteGameState
                liftIO $ writeChan replies (viewGS gs)

wireElements :: Process ()
wireElements = do
    requests <- liftIO $ C.newChan -- for sending commands to the remote agent
    replies  <- liftIO $ C.newChan -- for getting the state to the GUI
    let ui = (AbstractUI requests replies)
    seed <- liftIO $ getStdGen
    stageId <- spawnLocal $ P.initializeGame seed
    spawnLocal $ masterProc ui stageId
    liftIO $ createGUI ui

main :: IO()
main = do
    -- Model and process spawn
    seed <- getStdGen 

    -- Actors
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node wireElements
