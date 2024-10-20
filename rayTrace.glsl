#pragma language glsl4

layout (local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

#extension GL_NV_ray_tracing : require 

#define SKIP_VIEW_Z 1
#define SKIP_GET_POSITION_DATA 1

#include "functions.glsl"

#define PI 3.1415926535897932384626433832795
#define saturate(x)        clamp(x, 0.0, 1.0)

#define MIN_PERCEPTUAL_ROUGHNESS 0.045
#define MIN_ROUGHNESS 0.002025

#define MEDIUMP_FLT_MAX    65504.0
#define saturateMediump(x) min(x, MEDIUMP_FLT_MAX)

struct TextureInfo {
    vec2 scale;
};

struct Material {
    ivec2 albedo; // albedo, dummy
    ivec2 material;
    ivec2 materialIndices;
};

struct PackedTriangle {
    float data0;
    float data1;
    float data2;

    float data3;
    float data4;
    float data5;

    float data6;
    float data7;
    float data8;
};

struct TriangleData {
    uvec3 UVs;
    uint material;
    ivec3 normals;
    uint padding;
};

struct BVHNode {
    vec3 Min;
    vec3 Max;
    uint TriangleStart;
    uint TriangleCount;
};

struct RayInfo {
    vec3 origin;
    vec3 direction;
    vec3 color;
    vec3 incomingLight;
};

layout(std430, binding = 0) readonly restrict buffer Triangles {
    PackedTriangle triangles[];
};

layout(std430, binding = 1) readonly restrict buffer TriangleDatas {
    TriangleData triangleDatas[];
};

layout(std430, binding = 2) readonly restrict buffer Materials {
    Material materials[];
};

layout(std430, binding = 3) readonly restrict buffer BVHNodes {
    BVHNode nodes[];
};

/// Textures ///
uniform lowp sampler2DArray AlbedoTexture;
uniform lowp sampler2DArray EmissiveTexture;
uniform lowp sampler2DArray NormalTexture;
uniform lowp sampler2DArray MetallicRoughnessTexture;

layout(std430, binding = 3) readonly restrict buffer AlbedoBuffer {
    TextureInfo albedoScales[];
};

layout(std430, binding = 4) readonly restrict buffer EmissiveBuffer {
    TextureInfo emissiveScales[];
};

layout(std430, binding = 5) readonly restrict buffer NormalBuffer {
    TextureInfo normalScales[];
};

layout(std430, binding = 6) readonly restrict buffer MetallicRoughnessBuffer {
    TextureInfo metallicRoughnessScales[];
};

layout(std430, binding = 7) restrict buffer RayInfoBuffer {
    RayInfo rayInfos[];
};

/// Textures ///

struct Ray {
    vec3 origin;
    vec3 direction;
    vec3 invDirection;
};

float fastLog(float x)
{
    uint bx = floatBitsToUint(x);
    uint ex = bx >> 23u;
    int t = int(ex) - 127;
    uint s = abs(t);

    bx = 1065353216u | (bx & 8388607u);
    x = uintBitsToFloat(bx);

    return -1.49278 + (2.11263 + (-0.729104 + 0.10969 * x) * x) * x + 0.6931471806 * float(t);
}

float RandomValue(inout uint state) {
    state = state * 747796405u + 2891336453u;
    uint result = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    result = (result >> 22) ^ result;
    return float(result) / 4294967295.0;
}

const float PI2 = 2.0 * PI;

float RandomNormalDistributionValue(inout uint state) {
    float t = PI2 * RandomValue(state);
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

float rayBoxDistance(vec3 bmin, vec3 bmax, Ray ray) {
    vec3 tMin = (bmin - ray.origin) * ray.invDirection;
    vec3 tMax = (bmax - ray.origin) * ray.invDirection;

    vec3 t1 = min(tMin, tMax);
    float tNear = max(max(t1.x, t1.y), t1.z);

    vec3 t2 = max(tMin, tMax);
    float tFar = min(min(t2.x, t2.y), t2.z);

    return max(mix(1E7, tNear, tFar >= tNear && tFar > 0.0), 0.0);
}

vec3 rayTriangle(Ray ray, uint triIndex) {
    PackedTriangle triangle = triangles[triIndex];

    vec3 A = vec3(triangle.data0, triangle.data1, triangle.data2);
    vec3 AB = vec3(triangle.data3, triangle.data4, triangle.data5) - A;
    vec3 AC = vec3(triangle.data6, triangle.data7, triangle.data8) - A;
    vec3 normal = cross(AB, AC);
    vec3 AO = ray.origin - A;
    vec3 DAO = cross(AO, ray.direction);

    float det = -dot(ray.direction, normal);
    float invDet = 1.0 / det;

    float dist = dot(AO, normal) * invDet;
    float u = dot(AC, DAO) * invDet;
    float v = -dot(AB, DAO) * invDet;
    float w = 1.0 - u - v;

    float hit = float(dist >= 0.0 && u >= 0.0 && v >= 0.0 && w >= 0.0);
    
    return vec3(mix(-1.0, dist, hit), u, v);
}

#define MAX_DEPTH 28

vec3 RayTriangleBVH(Ray ray, out uint closestTriIndex)
{
    uint stack[MAX_DEPTH];
    int stackIndex = 0;
    stack[stackIndex++] = 0u;
    vec3 closestDistData = vec3(1E6);
    vec2 closestUV = vec2(0.0);

    closestTriIndex = 0u;

    int max = 1000;

    while (stackIndex > 0 && max--> 0 && stackIndex < MAX_DEPTH)
    {
        BVHNode node = nodes[stack[--stackIndex]];
        bool isLeaf = node.TriangleCount > 0u;

        if (isLeaf)
        {
            for (uint i = 0u; i < node.TriangleCount; i++)
            {
                vec3 distData = rayTriangle(ray, node.TriangleStart + i);

                if (distData.x > 0.0 && distData.x < closestDistData.x)
                {
                    closestDistData = distData;
                    closestTriIndex = node.TriangleStart + i;
                }
            }
        }
        else
        {
            uint childIndexA = node.TriangleStart;
            uint childIndexB = node.TriangleStart + 1u;
            BVHNode childA = nodes[childIndexA];
            float dstA = rayBoxDistance(childA.Min, childA.Max, ray);

            BVHNode childB = nodes[childIndexB];
            float dstB = rayBoxDistance(childB.Min, childB.Max, ray);
            
            // We want to look at closest child node first, so push it last
            bool isNearestA = dstA <= dstB;
            float dstNear = isNearestA ? dstA : dstB;
            float dstFar = isNearestA ? dstB : dstA;
            uint childIndexNear = isNearestA ? childIndexA : childIndexB;
            uint childIndexFar = isNearestA ? childIndexB : childIndexA;

            if (dstFar < closestDistData.x) stack[stackIndex++] = childIndexFar;
            if (dstNear < closestDistData.x) stack[stackIndex++] = childIndexNear;
        }
    }

    return closestDistData;
}

vec3 checkRayIntersections(Ray ray, out uint closestTriIndex)
{
    // this function will be filled in later once i have separate models
    vec3 closestDistData = RayTriangleBVH(ray, closestTriIndex);

    return closestDistData;
}

uniform mediump samplerCube Skybox;
uniform highp vec3 CameraPosition;
uniform highp mat4 InverseViewProjectionMatrix;

uniform float SkyboxBrightness;

vec3 sampleSkybox(Ray ray)
{
    return texture(Skybox, ray.direction).rgb * SkyboxBrightness;
}

const float GlobalFogDensity = 0.0;
const vec3 GlobalFogColor = vec3(1.0, 1.0, 1.0);

void traceRay(inout RayInfo ray, inout uint state) {
    vec3 rayIncomingLight = vec3(0.0);

    uint triangleIndex;

    Ray rayData = Ray(ray.origin, ray.direction, 1.0 / ray.direction);

    vec3 distData = checkRayIntersections(rayData, triangleIndex);

    if (distData.x >= 1E6) {
        ray.incomingLight += ray.color * sampleSkybox(rayData);
        ray.color *= 0.0; // ray has hit nothing, so it's done
        return;
    }

    if (exp(-GlobalFogDensity * distData.x) < RandomValue(state))
    {
        ray.color *= GlobalFogColor;

        distData.x *= RandomValue(state);
        ray.origin += ray.direction * distData.x;

        ray.direction = RandomDirection(state);

        return;
    }

    float u = distData.y;
    float v = distData.z;
    float w = 1.0 - u - v;

    ray.origin += ray.direction * (distData.x - 0.0001);

    TriangleData triangle = triangleDatas[triangleIndex];
    uint materialIndex = triangle.material;

    vec3 normal = UnpackNormalInt32(triangle.normals.x) * w + UnpackNormalInt32(triangle.normals.y) * u + UnpackNormalInt32(triangle.normals.z) * v;
    vec2 uv = w * unpackHalf2x16(triangle.UVs.x) + u * unpackHalf2x16(triangle.UVs.y) + v * unpackHalf2x16(triangle.UVs.z);

    // Material data

    Material material = materials[materialIndex];

    ivec4 indices = UnpackInt16Vec4Int32vec2(material.materialIndices);

    vec3 albedo = UnpackUnormVec3Int32(material.albedo.x);
    if (indices.x >= 0)
    {
        vec2 scale = albedoScales[indices.x].scale;
        albedo = texture(AlbedoTexture, vec3(uv * scale, indices.x)).rgb;
    }
    
    vec3 emissive = vec3(unpackHalf2x16(material.material.x), unpackHalf2x16(material.material.y).x);
    if (indices.y >= 0)
    {
        vec2 scale = emissiveScales[indices.y].scale;
        emissive *= texture(EmissiveTexture, vec3(uv * scale, indices.y)).rgb;
    }

    // material.material.y: 16emmisive.b, 8roughness unorm8, 8metallic unorm8
    float perceptualRoughness = (material.material.y >> 16) & 0xFF;
    float metallic = (material.material.y >> 24) & 0xFF;
    if (indices.w >= 0)
    {
        vec2 scale = metallicRoughnessScales[indices.w].scale;
        vec2 data = texture(MetallicRoughnessTexture, vec3(uv * scale, indices.w)).gb;
        perceptualRoughness = max(MIN_PERCEPTUAL_ROUGHNESS, data.x);
        metallic = data.y;
    }

    // Material data

    ray.incomingLight += emissive * ray.color;

    float roughness = perceptualRoughness * perceptualRoughness;
    
    if (RandomValue(state) > metallic)
    {
        ray.direction = RandomHemisphereDirection(normal, state);
    }
    else
    {
        vec3 specularDirection = reflect(ray.direction, normal);
        ray.direction = mix(specularDirection, RandomHemisphereDirection(specularDirection, state), roughness);
        ray.direction *= sign(dot(ray.direction, normal));
        ray.direction = normalize(ray.direction);
    }

    ray.color *= albedo;
}

uniform highp uint RandomIndex;

void computemain() {
    uint pixelIndex = gl_GlobalInvocationID.x;
    uint rngState = pixelIndex + RandomIndex;

    RayInfo rayInfo = rayInfos[pixelIndex];

    // if (max(max(rayInfo.color.r, rayInfo.color.g), rayInfo.color.b) < 0.001)
    // {
    //     return;
    // }

    traceRay(rayInfo, rngState);

    rayInfos[pixelIndex] = rayInfo;
}