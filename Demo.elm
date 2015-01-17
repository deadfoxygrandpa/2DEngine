module Demo where

import Color
import Signal ((<~))

import Engine

texture = Engine.texture "font.png"

scene t =
    case t of
        Just t' -> [Engine.texturedTile Color.purple t' (1, 0)]
        Nothing -> []

main = Engine.display (5, 5) (64, 64) <~ (scene <~ texture)
