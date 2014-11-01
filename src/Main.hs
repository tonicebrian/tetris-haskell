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
    liftIO $ putStrLn $ "El pid del master es " ++ show masterId
    forever $ do
        cmd <- liftIO $ C.readChan requests
        liftIO $ putStrLn ("Recibido un " ++ show cmd)
        case cmd of
            MoveLeft  -> send slaveId P.MoveLeft
            MoveRight -> send slaveId P.MoveRight
            RotateCW  -> send slaveId P.RotateCW
            Tick      -> send slaveId P.Tick
            Drop      -> send slaveId P.Drop
            View      -> do
                liftIO $ putStrLn "Antes de enviar el mensaje al agente"
                send slaveId $ P.RemoteView masterId
                liftIO $ putStrLn "Esperando a recibir el mensaje de vuelta"
                --(P.RGS gs) <- expect :: Process P.RemoteGameState
                algo <- expect
                case algo of
                    (P.RGS gs) -> liftIO $ writeChan replies (viewGS gs)
                    _          -> liftIO $ putStrLn "Recibida otra cosa de vuelta"
                liftIO $ putStrLn "Recibido el mensaje de vuelta y metiendo la respueta en el canal de replies"


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
