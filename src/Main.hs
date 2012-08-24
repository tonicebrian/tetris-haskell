module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.Rendering.Cairo

import Control.Monad.State as MS
import Control.Concurrent.MVar

import AbstractUI

main :: IO()
main = do
    let bluishGray = Color (256*48) (256*99) (256*99)
        bluishSilver = Color (256*210) (256*255) (256*255)

    -- Model
    ui <- newMVar "" 

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
    st <- liftIO $ readMVar ui
    liftIO $ renderWithDrawable drawin (render st)

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
     32    -> liftIO $ putMVar ui (keyName key) -- Space
     65362 -> liftIO $ putMVar ui (keyName key) -- Up
     65364 -> liftIO $ putMVar ui (keyName key) -- Down
     65361 -> liftIO $ putMVar ui (keyName key) -- Left
     65363 -> liftIO $ putMVar ui (keyName key) -- Right
     _     -> liftIO $ putMVar ui st

render :: String -> Render()
render message = do
    setSourceRGB 210 255 255
    moveTo 10 10
    showText message
    stroke

