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

layout(std430, binding = 0) writeonly restrict buffer RayInfoBuffer {
    RayInfo rayInfos[];
};

float RandomValue(inout uint state) {
    state = state * uint(747796405) + uint(2891336453);
    uint result = ((state >> ((state >> 28) + uint(4))) ^ state) * uint(277803737);
    result = (result >> 22) ^ result;
    return float(result) / 4294967295.0;
}

float RandomNormalDistributionValue(inout uint state) {
    float t = 2.0 * PI * RandomValue(state);
    float r = sqrt(-2.0 * log(RandomValue(state)));
    return r * cos(t);
}

vec3 RandomDirection(inout uint state)
{
    return normalize(vec3(RandomNormalDistributionValue(state),RandomNormalDistributionValue(state),RandomNormalDistributionValue(state)));
}

vec3 RandomHemisphereDirection(vec3 normal, inout uint state)
{
    vec3 dir = RandomDirection(state);
    return dir * sign(dot(normal,dir));
}

uniform highp vec3 CameraPosition;
uniform highp mat4 InverseViewProjectionMatrix;

uniform highp uint RandomIndex;

uniform highp uvec2 ScreenSize;

void computemain() {
    vec2 screen_coords = vec2(gl_GlobalInvocationID.xy);

    vec2 VarVertexCoord = (screen_coords + vec2(0.5)) / ScreenSize;

    vec4 world = mulVec3Matrix4x4(InverseViewProjectionMatrix, vec3(VarVertexCoord * 2.0 - 1.0, 1.0));

    vec3 rayDirection = normalize(world.xyz / world.w - CameraPosition);

    uint pixelIndex = uint(screen_coords.y * ScreenSize.x + screen_coords.x);
    uint rngState = pixelIndex + RandomIndex;

    vec3 randomDirection = RandomHemisphereDirection(rayDirection, rngState);

    const float jitterAmount = 0.002;

    rayDirection = normalize(mix(rayDirection, randomDirection, jitterAmount));

    RayInfo rayInfo = RayInfo(CameraPosition, rayDirection, vec3(1.0), vec3(0.0));

    rayInfos[pixelIndex] = rayInfo;
}