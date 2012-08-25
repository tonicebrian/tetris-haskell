module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC hiding (fill)
import Graphics.Rendering.Cairo

import Control.Monad.State as MS
import Control.Concurrent.MVar

import Debug.Trace

import AbstractUI
import Core

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
    -- Model
    ui <- newMVar emptyUI

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

    -- Events and callbacks
    window `on` deleteEvent $ tryEvent (liftIO mainQuit) 
    canvas `on` exposeEvent $ tryEvent (exposeHandler ui drawin)
    window `on` keyPressEvent $ tryEvent (keyPressHandler ui canvas)

    -- Main loop
    mainGUI
    
-- Handlers
-- Redraw handler 
exposeHandler :: MVar AbstractUI -> DrawWindow ->  EventM EExpose ()
exposeHandler ui drawin = do
    content <- liftIO $ readMVar ui
    liftIO $ renderWithDrawable drawin (render content)

-- Handles all the keyboard interactions
keyPressHandler :: WidgetClass a => MVar AbstractUI -> a -> EventM EKey ()
keyPressHandler mvs drawin = do
   key <- eventKeyVal
   liftIO $ updateModel mvs key
   liftIO $ widgetQueueDraw drawin

-- Changes the Abstract View
updateModel :: MVar AbstractUI -> KeyVal -> IO ()
updateModel ui key = do
   st <- takeMVar ui
   case key of
     32    -> liftIO $ putMVar ui st -- Space
     65362 -> liftIO $ putMVar ui st -- Up
     65364 -> liftIO $ putMVar ui st -- Down
     65361 -> liftIO $ putMVar ui st -- Left
     65363 -> liftIO $ putMVar ui st -- Right
     _     -> liftIO $ putMVar ui st

render :: AbstractUI -> Render()
render ui = do
    let gv = view ui
    drawEmptyGrid gv
    drawBlocks gv

drawCurrent :: GameView -> Render()
drawCurrent gv = do
    setBluishSilver
    let state = current gv
    paintBlocks gv state

drawBlocks :: GameView -> Render()
drawBlocks gv = do
    setBluishEvenLighter
    let state = blocks gv
    paintBlocks gv state

paintBlocks :: GameView -> [Block] -> Render()
paintBlocks gv bs = do
    mapM_ (\b -> let (x,y) = pos b
                 in buildRectangle gv (fromIntegral x) (fromIntegral y)) bs
    fill

drawEmptyGrid :: GameView -> Render ()
drawEmptyGrid gv = do
    setBluishLighterGray   
    setLineWidth 1
    let coords = [(fromIntegral x, fromIntegral y) | x <- [0..fst(gridSize gv)], y <- [0..snd(gridSize gv)]]
        recs = map (\(x,y) -> buildRectangle gv x y) coords 
    sequence_ recs
    stroke

buildRectangle :: GameView -> Double -> Double -> Render()
buildRectangle gv x y = rectangle x0 y0 width height
    where 
        (a,b) = gridSize gv
        x0 = x*blockSize
        y0 = (fromIntegral b-y)*blockSize
        width = blockSize
        height = blockSize

