module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC hiding (fill)
import Graphics.Rendering.Cairo

import Control.Monad.State as MS
import Control.Concurrent.MVar
import Control.Concurrent

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import System.Random

import AbstractUI
import Core
import Core.Game

-- Constants
bluishGray = Color (256*48) (256*99) (256*99)
bluishSilver = Color (256*210) (256*255) (256*255)
blockSize = 16 
blockMargin = 1

setBluishLighterGray = setSourceRGB (79/256) (130/256) (130/256)
setBluishGray = setSourceRGB (48/256) (99/256) (99/256)
setBluishEvenLighter = setSourceRGB (145/256) (196/256) (196/256)
setBluishSilver = setSourceRGB (210/256) (255/256) (255/256)

 
main :: IO()
main = do
    -- Model and process spawn
    seed <- getStdGen 
    Right transport <- createTransport "127.0.0.1" "8080" defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    pid <- forkProcess node (mkUI seed)
    let ui = AUI pid

    -- Every so often, we try to run other threads.
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100

    -- GUI components
    initGUI
    window <- windowNew
    set window [windowTitle := "Tetrix",
                windowDefaultWidth := 700, windowDefaultHeight := 400]
    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas
    widgetModifyBg canvas StateNormal bluishGray

    -- Show and run
    widgetShowAll window
    drawin <- widgetGetDrawWindow canvas

    -- Create the lightweight thread that controls ticking
    forkIO (tickUI ui canvas)

    -- Events and callbacks
    window `on` deleteEvent $ tryEvent (liftIO mainQuit) 
    canvas `on` exposeEvent $ tryEvent (exposeHandler ui drawin)
    window `on` keyPressEvent $ tryEvent (keyPressHandler ui canvas)

    -- Main loop
    mainGUI


-- Ticking functions
tickUI :: AbstractUI -> DrawingArea -> IO()
tickUI ui canvas = do
    threadDelay 1000000
    --TODO runProcess node (tick ui) 
    postGUIAsync $ widgetQueueDraw canvas
    tickUI ui canvas

-- Handlers
-- Redraw handler 
exposeHandler :: AbstractUI -> DrawWindow ->  EventM EExpose ()
exposeHandler ui drawin = do
    -- TODO
    --runProcess node (do
    --                  pid <- getSelfPid
    --                  send pend 
    --                 -- Wait until a message is received
    --                )
    liftIO $ renderWithDrawable drawin (render undefined) -- TODO

-- Handles all the keyboard interactions
keyPressHandler :: WidgetClass a => AbstractUI -> a -> EventM EKey ()
keyPressHandler ui drawin = do
   key <- eventKeyVal
   liftIO $ updateModel ui key
   liftIO $ widgetQueueDraw drawin

-- Changes the Abstract View
updateModel :: AbstractUI -> KeyVal -> IO ()
updateModel ui key = do
   let p = case key of
             32    -> dropPiece ui -- Space
             65362 -> rotateCW ui -- Up
             65364 -> tick ui -- Down
             65361 -> left ui -- Left
             65363 -> right ui -- Right
       pid = processId ui
   --runProcess (processNodeId pid) p
   putStrLn "TODO This has to be changed"
   

drawBoard :: (Int,Int) -> (Int,Int) -> [Block] -> [Block] -> Render()
drawBoard (offx,offy) size blks cp = do
    drawEmptyGrid size
    drawBlocks size blks
    drawCurrent size blks
  where
        drawCurrent :: (Int,Int) -> [Block] -> Render()
        drawCurrent tam bs = do
            setBluishSilver
            paintBlocks tam bs
        
        drawBlocks :: (Int,Int) -> [Block] -> Render()
        drawBlocks size blks = do
            setBluishEvenLighter
            paintBlocks size blks
        
        paintBlocks :: (Int,Int) -> [Block] -> Render()
        paintBlocks (a,b) bs = do
            mapM_ (\blk -> let (x,y) = posBlock blk
                           in buildRectangle (a,b) (fromIntegral x) (fromIntegral y)) bs
            fill
        
        drawEmptyGrid :: (Int,Int) -> Render ()
        drawEmptyGrid (a,b) = do
            setBluishLighterGray   
            setLineWidth 1
            let coords = [(fromIntegral x, fromIntegral y) | x <- [0..(a-1)], y <- [0..(b-1)]]
                recs = map (\(x,y) -> buildRectangle (a,b) x y) coords 
            sequence_ recs
            stroke
        
        buildRectangle :: (Int,Int) -> Double -> Double -> Render()
        buildRectangle (a,b) x y = rectangle x0 y0 width height
            where 
                x0 = (x+fromIntegral offx)*blockSize
                y0 = (fromIntegral (b+offy)-y)*blockSize
                width = blockSize
                height = blockSize
   
    
render :: GameView -> Render()
render gv = do
    let size = gridSizeGV gv
        blks = blocksGV gv
        cp = currentGV gv
        next = nextGV gv
    drawBoard (0,0) size blks cp
    drawBoard (12,0) (4,4) next []
    case statusGV gv of
        GameOver -> do
            setBluishSilver
            layout <- createLayout "Game Over!!!"
            showLayout layout
        Active -> return ()


