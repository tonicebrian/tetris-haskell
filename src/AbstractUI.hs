module AbstractUI(
        -- The View
        AbstractUI,

        -- Operations on the view
        left,
        right,
        up,
        down,
        space
) where

type AbstractUI = String

left,right,up,down,space :: AbstractUI -> AbstractUI
left ui  = "left"
right ui = "right"
up ui    = "up"
down ui  = "down"
space ui = "space"




