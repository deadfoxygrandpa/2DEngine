module Demo where

import Color
import Time (fps)
import Signal ((<~), (~))
import Graphics.Element (color, flow, down, Element)
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Signal

import Engine
import BMFont

texture = Engine.texture "font2_0.png"

sprite t x y = Engine.spriteTile 261 55 26 44 Color.orange
    t
    (0, 0)

tex t _ _ = Engine.texturedTile Color.orange t (0, 0)

txt t s = BMFont.write s Color.red t (-25, 0)

scene t' s = case t' of
    Just t -> txt t s
    Nothing -> []

msg : Signal.Channel Field.Content
msg = Signal.channel Field.noContent

msgString : Signal.Signal String
msgString = .string <~ Signal.subscribe msg

msgField : Signal.Signal Element
msgField = Field.field Field.defaultStyle (Signal.send msg) "type here" <~ Signal.subscribe msg

main = (\texture field s -> flow down [color Color.purple <| Engine.display (50, 1) (15, 32) (scene texture s), field]) <~ texture ~ msgField ~ msgString
