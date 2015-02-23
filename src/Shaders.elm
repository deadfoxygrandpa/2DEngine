module Shaders where

import Math.Vector2 (Vec2, vec2)
import Math.Vector3 (..)
import Math.Vector4 (Vec4, vec4)
import Math.Matrix4 (..)
import WebGL (..)


-- Shaders for making a solid color tile

solidColorVertexShader : Shader { attr | position : Vec2 }
                                { unif | perspective : Mat4, offset : Vec2 }
                                {}
solidColorVertexShader = [glsl|

attribute vec2 position;
uniform mat4 perspective;
uniform vec2 offset;

void main () {
    vec2 pos2D = (2.0 * offset) + position;
    gl_Position = perspective * vec4(pos2D, 0.0, 1.0);
}

|]

solidColorFragmentShader : Shader {}
                                  { unif | color : Vec4 }
                                  {}
solidColorFragmentShader = [glsl|

precision mediump float;
uniform vec4 color;

void main () {
    if (color.a == 0.0) {
        discard;
    } else {
        gl_FragColor = color;
    }
}

|]

-- Shaders for making a textured tile

texturedVertexShader : Shader { attr | position : Vec2, textureCoord : Vec2 }
                              { unif | perspective : Mat4, offset : Vec2 }
                              { vcoord : Vec2 }
texturedVertexShader = [glsl|

attribute vec2 position;
attribute vec2 textureCoord;
uniform mat4 perspective;
uniform vec2 offset;
varying vec2 vcoord;

void main () {
    vec2 pos2D = (2.0 * offset) + position;
    gl_Position = perspective * vec4(pos2D, 0.0, 1.0);
    vcoord = textureCoord;
}

|]

texturedFragmentShader : Shader {}
                                { unif | color : Vec4, texture : Texture }
                                { vcoord : Vec2 }
texturedFragmentShader = [glsl|

precision mediump float;
uniform vec4 color;
uniform sampler2D texture;
varying vec2 vcoord;

void main () {
    vec4 tcolor = texture2D(texture, vec2(vcoord.x, -vcoord.y));
    if (tcolor.a < 0.1) {
        discard;
    } else {
        gl_FragColor = color * tcolor;
    }
}

|]

spriteFragmentShader : Shader {}
                              { unif | color : Vec4, texture : Texture, size : Vec3, sprite : Vec3, sprite2 : Vec3 }
                              { vcoord : Vec2 }
spriteFragmentShader = [glsl|

precision mediump float;
uniform vec4 color;
uniform sampler2D texture;
uniform vec3 size;
uniform vec3 sprite;
uniform vec3 sprite2;
varying vec2 vcoord;

void main () {
    float xScale = size.x / sprite2.x;
    float yScale = size.y / sprite2.y;
    float y = 1.0 - sprite.y;
    vec2 spritecoord = vec2(sprite.x / size.x, y / size.y);
    vec2 coord = vec2(spritecoord.x, spritecoord.y) + (vcoord / vec2(xScale, -yScale));
    vec4 tcolor = texture2D(texture, coord);
    if (tcolor.a == 0.0) {
        discard;
    } else {
        gl_FragColor = color;
    }
}

|]
