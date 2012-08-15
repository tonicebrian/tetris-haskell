module Main(Main.main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

main :: IO()
main = do
    let bluishGray = Color 48 99 99
        bluishSilver = Color 210 255 255
    initGUI
    dia <- dialogNew
    contain <- dialogGetUpper dia
    canvas <- drawingAreaNew
    canvas `on` sizeRequest $ return (Requisition 700 400)

    widgetShowAll dia
    mainGUI
    
