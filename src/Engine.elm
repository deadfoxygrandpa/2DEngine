module Engine where

import Color (Color)
import Graphics.Element (Element)
import List
import Signal ((<~), Signal)

import Math.Vector2 (Vec2, vec2)
import Math.Vector3 (..)
import Math.Vector4 (Vec4, vec4)
import Math.Matrix4 (..)
import WebGL

import Types (..)
import Helpers (..)
import Shaders

baseTile : List (WebGL.Triangle Vertex)
baseTile = quad (-1, 1) (1, 1) (-1, -1) (1, -1)

spriteTile : Int -> Int -> Int -> Int -> Color -> Texture -> (Int, Int) -> Tile
spriteTile x y w h color texture (xOffset, yOffset) perspective =
    spriteTile2 x y w h color texture (toFloat xOffset, toFloat yOffset) perspective

spriteTile2 : Int -> Int -> Int -> Int -> Color -> Texture -> (Float, Float) -> Tile
spriteTile2 x y w h color texture offset perspective =
    let (x', y') = (toFloat x, toFloat y)
        v3 (a, b) = vec3 (toFloat a) (toFloat b) 0
    in
        WebGL.entity
            Shaders.texturedVertexShader
            Shaders.spriteFragmentShader
            baseTile
            { perspective = perspective
            , offset = makeOffset2 offset
            , color = fromRGB color
            , texture = texture
            , size = v3 (size texture)
            , sprite = v3 (x, y)
            , sprite2 = v3 (w, h)
            }

texturedTile : Color -> Texture -> (Int, Int) -> Tile
texturedTile color texture offset perspective =
    WebGL.entity
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
    in
        WebGL.entity
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
    in
        WebGL.webgl dimensions <| List.map (\tile -> tile perspective) tiles

makeOffset : (Int, Int) -> Vec2
makeOffset (x, y) = makeOffset2 ((toFloat x), (toFloat y))

makeOffset2 : (Float, Float) -> Vec2
makeOffset2 (x, y) = vec2 x y

texture : String -> Signal (Maybe Texture)
texture url = responseToMaybe <~ WebGL.loadTexture url

size _ = (512, 512)
