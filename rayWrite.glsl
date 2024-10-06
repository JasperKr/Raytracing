#pragma language glsl4

layout (local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

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
};

layout(std430, binding = 0) readonly restrict buffer RayInfoBuffer {
    RayInfo rayInfos[];
};

layout(rgba32f, binding = 1) uniform highp image2D CurrentFrame;

uniform highp uint FrameIndex;
uniform highp uvec2 ScreenSize;

void computemain() {
    vec2 screen_coords = vec2(gl_GlobalInvocationID.xy);

    uint pixelIndex = uint(screen_coords.y * ScreenSize.x + screen_coords.x);

    RayInfo rayInfo = rayInfos[pixelIndex];

    highp float contribution = 1.0 / float(FrameIndex);

    highp vec3 previousColor = imageLoad(CurrentFrame, ivec2(screen_coords)).rgb;

    imageStore(CurrentFrame, ivec2(screen_coords), vec4(mix(previousColor, rayInfo.incomingLight, contribution), 1.0));
}