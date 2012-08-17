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

    -- Events and callbacks
    window `onDestroy` mainQuit

    -- Show and run
    widgetShowAll window

    drawin <- widgetGetDrawWindow canvas

    -- TODO. Functions onXXX are deprecated see what is the proper way to do
    -- this using the pattern window `on` keyPressedEvent (exposeEvent)
    onExpose canvas $ \_ -> do 
                                st <- readMVar ui
                                putStrLn st
                                myPaint st drawin


    --window `on` keyPressEvent $ hKeyPress ui >>= liftIO . putStrLn . eventTime >> return True
    onKeyPress window $ \_ -> hKeyPress ui >> return True

    mainGUI
    

keyPressHandler ui drawin = do
    hKeyPress ui

renderScene ui drawin = do 
    st <- readMVar ui
    putStrLn st
    myPaint st drawin

 
myPaint :: DrawableClass a => ViewState -> a -> IO Bool
myPaint st drawin = renderWithDrawable drawin (do
                                                   myDraw st
                                                   return True
                                                )

-- Handles all the keyboard interactions
hKeyPress :: MVar ViewState -> EventM EKey ()
hKeyPress mvs = do
   let print = liftIO . putStrLn . keyName 
   key <- eventKeyVal
   case key of
     32     -> print key
     65362  -> print key
     65364  -> print key
     65361  -> print key
     65363  -> print key


myDraw :: String -> Render()
myDraw message = do
    setSourceRGB 0 0 0
    moveTo 100 100
    showText message
    stroke

        
