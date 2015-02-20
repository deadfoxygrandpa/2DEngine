module Demo where

import Color
import Time (fps)
import Signal ((<~))
import Graphics.Element (color)
import Debug

import Engine

texture = Engine.texture "font.png"

sprite t x y = Engine.spriteTile 261 55 26 44 Color.orange
    t
    (0, 0)

tex t _ _ = Engine.texturedTile Color.orange t (0, 0)

scene t' = case t' of
    Just t -> [sprite t 0 0]
    Nothing -> []

main = (\texture -> color Color.purple <| Engine.display (1, 1) (512, 512) (scene texture)) <~ texture
