#pragma language glsl4

#define SKIP_VIEW_Z 1
#define SKIP_GET_POSITION_DATA 1

#include "functions.glsl"

#define PI 3.1415926535897932384626433832795
#define saturate(x)        clamp(x, 0.0, 1.0)

#define MIN_PERCEPTUAL_ROUGHNESS 0.045
#define MIN_ROUGHNESS 0.002025

#define MEDIUMP_FLT_MAX    65504.0
#define saturateMediump(x) min(x, MEDIUMP_FLT_MAX)

struct RayInfo {
    vec3 origin;
    vec3 direction;
    vec3 color;
    vec3 incomingLight;
    float distance;
};

layout(std430, binding = 0) readonly restrict buffer RayInfoBuffer {
    RayInfo rayInfos[];
};

uniform highp sampler2D PreviousFrame;

uniform highp uint FrameIndex;
uniform highp vec2 ScreenSize;
uniform highp float Exposure;

uniform highp mat4 PreviousViewProjectionMatrix;
uniform highp vec3 CameraPosition;
uniform highp mat4 InverseViewProjectionMatrix;

#ifdef PIXEL

out vec4 FragColor;

void pixelmain() {
    vec2 uv = love_PixelCoord.xy / ScreenSize;
    uint pixelIndex = uint(floor(love_PixelCoord.y) * ScreenSize.x + floor(love_PixelCoord.x));

    RayInfo rayInfo = rayInfos[pixelIndex];

    highp float contribution = 1.0 / float(FrameIndex);

    highp vec3 previousColor = texture(PreviousFrame, uv).xyz;

    FragColor = vec4(mix(previousColor, rayInfo.incomingLight, contribution), 1.0);
}

#endif