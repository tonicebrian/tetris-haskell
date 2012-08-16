module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.Rendering.Cairo

import Control.Monad.State as MS
import AbstractUI

-- Just for reference
-- bluishGrayRGB = 48 99 99
-- bluishSilverRGB = 210 255 255

keySpace = 32 
keyUp    = 65362
keyDown  = 65364
keyLeft  = 65361
keyRight = 65363

main :: IO()
main = do
    let bluishGray = Color (256*48) (256*99) (256*99)
        bluishSilver = Color (256*210) (256*255) (256*255)
        
    initGUI

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
    window `on` keyPressEvent $ tryEvent hKeyPress

    mainGUI
    
-- Handles all the keyboard interactions
hKeyPress = do
   let print = liftIO . putStrLn . keyName 
   key <- eventKeyVal
   case key of
     32     -> print key
     65362  -> print key
     65364  -> print key
     65361  -> print key
     65363  -> print key

myDraw :: Double -> Double -> Render()
myDraw w h  = do
    setSourceRGB 1 1 1
    paint 

    setSourceRGB 210 255 255 
    setLineWidth 5

    moveTo (0.5 * w) (0.43 * h)
    lineTo (0.33 * w) (0.71 * h)
    lineTo (0.66 * w) (0.71 * h)
    closePath

    stroke
