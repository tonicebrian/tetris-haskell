module Main(Main.main) where


import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Node (initRemoteTable)
import Network.Transport.TCP

import System.Random

import Processes
import AbstractUI
import GUI

 
main :: IO()
main = do
    -- Model and process spawn
    seed <- getStdGen 

    -- Actors
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    pid <- forkProcess node (initializeGame seed)

    let ui = AUI pid
    createGUI ui

