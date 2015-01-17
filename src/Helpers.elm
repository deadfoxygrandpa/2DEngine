module Helpers where

import Color (Color, toRgb)
import Http (Response(..))
import List

import WebGL (Triangle)
import Math.Vector2 (vec2)
import Math.Vector4 (Vec4, vec4)

import Types (Point, Vertex)

quad : Point -> Point -> Point -> Point -> List (Triangle Vertex)
quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let topLeft     = Vertex (vec2 x1 y1) (vec2 0 0)
        topRight    = Vertex (vec2 x2 y2) (vec2 1 0)
        bottomLeft  = Vertex (vec2 x3 y3) (vec2 0 1)
        bottomRight = Vertex (vec2 x4 y4) (vec2 1 1)
    in  [ ( topLeft, topRight, bottomLeft)
        , ( bottomLeft, topRight, bottomRight)
        ]

fromRGB : Color -> Vec4
fromRGB color =
    let {red, green, blue, alpha} = toRgb color
        div x = toFloat x / 255
    in  vec4 (div red) (div green) (div blue) alpha

even : Int -> Bool
even n = n % 2 == 0

responseToMaybe : Response a -> Maybe a
responseToMaybe response =
    case response of
        Success a -> Just a
        _         -> Nothing

justs : List (Maybe a) -> List a
justs ms =
    let isJust m = case m of
                    Just _ -> True
                    Nothing -> False
        js = List.filter isJust ms
        just m = case m of
                    Just x -> x
    in List.map just js
