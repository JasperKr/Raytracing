#ifndef SKIP_VIEW_Z

float getViewZ(in float depth) {
    return cameras[0].NearMulFar / (cameras[0].FarMinusNear * depth - cameras[0].Far);
}

#endif

#define mulVec3Matrix4x4(m, a) (a.x * m[0] + (a.y * m[1] + (a.z * m[2] + m[3])))
#define mulVec3Matrix4x4W0(m, a) (a.x * m[0] + (a.y * m[1] + (a.z * m[2])))
#define lengthSqr(a) dot(a, a)
#define qInverse(q) vec4(-q.xyz, q.w)
#define rotate_vertex_position(vertex, quat) (vertex + 2.0 * cross(cross(vertex, quat.xyz) + quat.w * vertex, quat.xyz))

// vec4 mulVec3Matrix4x4(in mat4 m, in vec3 a) {
//     return a.x * m[0] + (a.y * m[1] + (a.z * m[2] + m[3]));
// }

// vec4 mulVec3Matrix4x4W0(in mat4 m, in vec3 a) {
//     return a.x * m[0] + (a.y * m[1] + (a.z * m[2]));
// }

#ifndef SKIP_GET_POSITION_DATA
vec3[3] getPositionData(in vec2 uv, in float depth) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;

    vec4 view = mulVec3Matrix4x4(cameras[0].InverseProjectionMatrix, clip);
    view.xyz /= view.w; // only divide x, y, z by w since we don't care about w
    vec4 world = mulVec3Matrix4x4(cameras[0].InverseViewMatrix, view.xyz); // we can ignore w since x / x = 1

    return vec3[3](clip, view.xyz, world.xyz);
}

vec3[3] getPositionData(in vec2 uv, in float depth, int cameraIndex) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;

    vec4 view = mulVec3Matrix4x4(cameras[cameraIndex].InverseProjectionMatrix, clip);
    view.xyz /= view.w; // only divide x, y, z by w since we don't care about w
    vec4 world = mulVec3Matrix4x4(cameras[cameraIndex].InverseViewMatrix, view.xyz); // we can ignore w since x / x = 1

    return vec3[3](clip, view.xyz, world.xyz);
}

vec3[2] getWorldPositionData(in vec2 uv, in float depth) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;

    vec4 view = mulVec3Matrix4x4(cameras[0].InverseProjectionMatrix, clip);
    view.xyz /= view.w; // only divide x, y, z by w since we don't care about w
    vec4 world = mulVec3Matrix4x4(cameras[0].InverseViewMatrix, view.xyz); // we can ignore w since x / x = 1

    return vec3[2](view.xyz, world.xyz);
}

vec3[2] getWorldPositionData(in vec2 uv, in float depth, int cameraIndex) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;

    vec4 view = mulVec3Matrix4x4(cameras[cameraIndex].InverseProjectionMatrix, clip);
    view.xyz /= view.w; // only divide x, y, z by w since we don't care about w
    vec4 world = mulVec3Matrix4x4(cameras[cameraIndex].InverseViewMatrix, view.xyz); // we can ignore w since x / x = 1

    return vec3[2](view.xyz, world.xyz);
}

vec3 reproject(vec3 position, int from, int to) {
    position = position * 2.0 - 1.0;
    vec4 world = mulVec3Matrix4x4(cameras[from].InverseViewProjectionMatrix, position);
    world.xyz /= world.w;

    vec4 clip = mulVec3Matrix4x4(cameras[to].ViewProjectionMatrix, world.xyz);
    clip.xyz /= clip.w;
    return clip.xyz * 0.5 + 0.5;
}

vec3 getForwardVector() {
    return normalize(-cameras[0].InverseViewMatrix[2].xyz);
}

vec3 getForwardVector(mat4 InverseViewMatrix) {
    return normalize(-InverseViewMatrix[2].xyz);
}

vec3 getRightVector() {
    return normalize(cameras[0].InverseViewMatrix[0].xyz);
}

vec3 getRightVector(mat4 InverseViewMatrix) {
    return normalize(InverseViewMatrix[0].xyz);
}

vec3 getUpVector() {
    return normalize(cameras[0].InverseViewMatrix[1].xyz);
}

vec3 getUpVector(mat4 InverseViewMatrix) {
    return normalize(InverseViewMatrix[1].xyz);
}

vec3 getWorldPosition(in vec2 uv, in float depth) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;

    vec4 world = mulVec3Matrix4x4(cameras[0].InverseViewProjectionMatrix, clip);
    world.xyz /= world.w;
    return world.xyz;
}

vec3 getWorldPosition(in vec2 uv, in float depth, int cameraIndex) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;

    vec4 world = mulVec3Matrix4x4(cameras[cameraIndex].InverseViewProjectionMatrix, clip);
    world.xyz /= world.w;
    return world.xyz;
}

vec3 getWorldPosition(vec2 uv, float depth, mat4 inverseViewProjectionMatrix) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;
    vec4 world = mulVec3Matrix4x4(inverseViewProjectionMatrix, clip);

    return world.xyz / world.w;
}

vec3 getViewPosition(in vec2 uv, in float depth) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;
    vec4 view = mulVec3Matrix4x4(cameras[0].InverseProjectionMatrix, clip);

    return view.xyz / view.w;
}

vec3 getViewPosition(in vec2 uv, in float depth, int cameraIndex) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;
    vec4 view = mulVec3Matrix4x4(cameras[cameraIndex].InverseProjectionMatrix, clip);

    return view.xyz / view.w;
}

vec3 getViewPosition(in vec2 uv, in float depth, mat4 inverseProjectionMatrix) {
    vec3 clip = vec3(uv, depth) * 2.0 - 1.0;
    vec4 view = mulVec3Matrix4x4(inverseProjectionMatrix, clip);

    return view.xyz / view.w;
}

#endif

ivec4 UnpackInt16Vec4Int32vec2(ivec2 data) {
    return ivec4(
        data.x & 0xFFFF,
        data.x >> 16,
        data.y & 0xFFFF,
        data.y >> 16
    );
}

vec3 UnpackUnormVec3Int32(int data) {
    return vec3(
        float((data >> 20) & 0x3FF) / 1023.0,
        float((data >> 10) & 0x3FF) / 1023.0,
        float(data & 0x3FF) / 1023.0
    );
}

vec4 UnpackTangentInt32(int data) {
    return vec4(
        normalize(UnpackUnormVec3Int32(data) * 2.0 - 1.0),
        float((data >> 31) & 1) * 2.0 - 1.0
    );
}

vec3 UnpackNormalInt32(int data) {
    return normalize(UnpackUnormVec3Int32(data) * 2.0 - 1.0);
}

int PackUnormVec3Int32(vec3 data) {
    return int(data.r * 1023.0) << 20 | int(data.g * 1023.0) << 10 | int(data.b * 1023.0);
}

mediump float packToInt8Unorm(bool[8] data) {
    int result = 0;
    for (int i = 0; i < 8; i++) {
        result += int(data[i]) << i;
    }
    return float(result) / 255.0;
}

bool[8] unpackFromInt8Unorm(float data) {
    bool[8] bools;
    int dataInt = int(floor(data * 255.0 + 0.5));
    for (int i = 0; i < 8; i++) {
        bools[i] = ((dataInt >> i) & 1) != 0;
    }
    return bools;
}

float clampNoV(float NoV) {
    // Neubelt and Pettineo 2013, "Crafting a Next-gen Material Pipeline for The Order: 1886"
    return max(NoV, 1e-4);
}

// The MIT License
// Copyright © 2013 Inigo Quilez
// https://www.youtube.com/c/InigoQuilez
// https://iquilezles.org/
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions: The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// This is the implementation for my article "improved texture interpolation"
// 
// https://iquilezles.org/articles/texture
//
// It shows how to get some smooth texture interpolation without resorting to the regular
// bicubic filtering, which is pretty expensive because it needs 9 texels instead of the 
// 4 the hardware uses for bilinear interpolation.
//
// With this techinque here, you can get smooth interpolation while still using only
// 1 bilinear fetche, by tricking the hardware. The idea is to get the fractional part
// of the texel coordinates and apply a smooth curve to it such that the derivatives are
// zero at the extremes. The regular cubic or quintic smoothstep functions are just
// perfect for this task.

vec4 textureNice(sampler2D sam, vec2 uv) {
    float textureResolution = float(textureSize(sam, 0).x);
    uv = uv * textureResolution + 0.5;
    vec2 iuv = floor(uv);
    vec2 fuv = fract(uv);
    uv = iuv + fuv * fuv * (3.0 - 2.0 * fuv);
    uv = (uv - 0.5) / textureResolution;
    return texture(sam, uv);
}

vec4 textureNice(sampler2D sam, vec2 uv, float mip) {
    float textureResolution = float(textureSize(sam, int(mip)).x);
    uv = uv * textureResolution + 0.5;
    vec2 iuv = floor(uv);
    vec2 fuv = fract(uv);
    uv = iuv + fuv * fuv * (3.0 - 2.0 * fuv);
    uv = (uv - 0.5) / textureResolution;
    return textureLod(sam, uv, mip);
}