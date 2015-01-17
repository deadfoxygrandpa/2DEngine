module Engine where

import Color (Color)
import Graphics.Element (Element)
import List
import Signal ((<~), Signal)

import Math.Vector2 (Vec2, vec2)
import Math.Vector3 (..)
import Math.Vector4 (Vec4, vec4)
import Math.Matrix4 (..)
import WebGL (..)

import Types (..)
import Helpers (..)
import Shaders

baseTile : List (Triangle Vertex)
baseTile = quad (-1, 1) (1, 1) (-1, -1) (1, -1)

spriteTile : Int -> Int -> Color -> Texture -> (Int, Int) -> Tile
spriteTile x y color texture offset perspective =
    let (x', y') = (toFloat x, toFloat y)
    in  entity
            Shaders.texturedVertexShader
            Shaders.spriteFragmentShader
            baseTile
            { perspective = perspective
            , offset = makeOffset offset
            , color = fromRGB color
            , texture = texture
            , sprite = vec3 x' y' 0
            }

texturedTile : Color -> Texture -> (Int, Int) -> Tile
texturedTile color texture offset perspective =
    entity
        Shaders.texturedVertexShader
        Shaders.texturedFragmentShader
        baseTile
        { perspective = perspective
        , offset = makeOffset offset
        , color = fromRGB color
        , texture = texture
        }

coloredTile : Color -> (Int, Int) -> Tile
coloredTile color offset perspective =
    let color' = fromRGB color
    in  entity
            Shaders.solidColorVertexShader
            Shaders.solidColorFragmentShader
            baseTile
            { perspective = perspective
            , offset = makeOffset offset
            , color = color'
            }

display : (Int, Int) -> (Float, Float) -> List Tile -> Element
display (w, h) (xScale, yScale) tiles =
    let (w', h') = (w // 2, h // 2)
        (left, right) = case even w of
                            True  -> (toFloat (-w - 1), toFloat w - 1)
                            False -> (toFloat (-w), toFloat w)
        (top, bottom) = case even h of
                            True  -> (toFloat (-h - 1), toFloat h - 1)
                            False -> (toFloat (-h), toFloat h)
        perspective = makeOrtho2D left right top bottom
        w'' = (toFloat w) * xScale |> round
        h'' = (toFloat h) * yScale |> round
        dimensions = (w'', h'')
    in webgl dimensions <| List.map (\tile -> tile perspective) tiles

makeOffset : (Int, Int) -> Vec2
makeOffset (x, y) = vec2 (toFloat x) (toFloat y)

texture : String -> Signal (Maybe Texture)
texture url = responseToMaybe <~ loadTexture url
