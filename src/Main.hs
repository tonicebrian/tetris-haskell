module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
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
    stroke

drawEmptyGrid :: GameView -> Render ()
drawEmptyGrid gv = do
    setSourceRGB 79 130 130
    setLineWidth 1
    let coords = [(fromIntegral x, fromIntegral y) | x <- [0..fst(gridSize gv)], y <- [0..snd(gridSize gv)]]
        recs = map (\(x,y) -> buildRectangle gv x y) coords 
    sequence_ recs

buildRectangle :: GameView -> Double -> Double -> Render()
buildRectangle gv x y = rectangle x0 y0 width height
    where 
        x0 = x*blockSize
        y0 = (y+1)*blockSize
        width = blockSize
        height = blockSize

