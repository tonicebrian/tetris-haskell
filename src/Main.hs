module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.Rendering.Cairo

import Control.Monad.State as MS
import Control.Concurrent.MVar

import AbstractUI

-- Just for reference
-- bluishGrayRGB = 48 99 99
-- bluishSilverRGB = 210 255 255

keySpace = 32 
keyUp    = 65362
keyDown  = 65364
keyLeft  = 65361
keyRight = 65363

type ViewState = String
type State = MVar ViewState

main :: IO()
main = do
    let bluishGray = Color (256*48) (256*99) (256*99)
        bluishSilver = Color (256*210) (256*255) (256*255)
        
    initGUI

    ui <- newMVar "left" 

    -- GUI components
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

    mainGUI
    

exposeHandler :: MVar ViewState -> DrawWindow ->  EventM EExpose ()
exposeHandler ui drawin = do
    st <- liftIO $ readMVar ui
    liftIO $ myPaint st drawin

 
myPaint :: DrawableClass a => ViewState -> a -> IO ()
myPaint st drawin = renderWithDrawable drawin (myDraw st)

-- Handles all the keyboard interactions
keyPressHandler :: WidgetClass a => MVar ViewState -> a -> EventM EKey ()
keyPressHandler mvs drawin = do
   let print = liftIO . putStrLn . keyName 
   key <- eventKeyVal
   case key of
     32     -> print key
     65362  -> print key
     65364  -> print key
     65361  -> print key
     65363  -> print key
   
   liftIO $ widgetQueueDraw drawin


myDraw :: String -> Render()
myDraw message = do
    setSourceRGB 0 0 0
    moveTo 100 100
    showText message
    stroke

        
