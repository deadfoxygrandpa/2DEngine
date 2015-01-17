module Types where

import WebGL (..)
import Math.Vector2 (Vec2)
import Math.Matrix4 (Mat4)

type alias Vertex = { position : Vec2, textureCoord : Vec2 }
type alias Point = (Float, Float)

type alias Tile = Mat4 -> Entity
