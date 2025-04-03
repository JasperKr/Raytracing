#pragma language glsl4

layout (local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

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
    result = (result >> 22u) ^ result;
    return float(result) / 4294967295.0;
}

float FastSqrt(float x) {
    return intBitsToFloat(0x1fbd1df5 + (floatBitsToInt(x) >> 1));
}

const float PI2 = 2.0 * PI;
const float INV_PI = 1.0 / PI;
const float INV_PI2 = 1.0 / PI2;
const float RndToRad = 1.0 / (4294967295.0 * INV_PI2);

float RandomNormalDistributionValue(inout uint state) {
    state = state * 747796405u + 2891336453u;
    uint result = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    result = (result >> 22u) ^ result;
    float t = float(result) * RndToRad;

    float r = FastSqrt(-2.0 * fastLog(RandomValue(state)));
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

void GetNodeBounds(uvec3 data, inout vec3 min, inout vec3 max) {
    vec2 a = unpackHalf2x16(data.x);
    vec2 b = unpackHalf2x16(data.y);
    min = vec3(a.xy, b.x);
    vec2 c = unpackHalf2x16(data.z);
    max = vec3(b.y, c.xy);
}

#define MAX_DEPTH 32

vec3 RayTriangleBVH(Ray ray, out uint closestTriIndex, out bool frontFacing)
{
    uint stack[MAX_DEPTH];
    int stackIndex = 0;
    stack[stackIndex++] = 0u;
    vec3 closestDistData = vec3(1E6);
    vec2 closestUV = vec2(0.0);

    closestTriIndex = 0u;

    int maxIter = 1000;

    while (stackIndex > 0 && maxIter--> 0 && stackIndex < MAX_DEPTH)
    {
        BVHNode node = nodes[stack[--stackIndex]];

        uint triangleStart = node.TriangleStart;
        uint triangleCount = node.TriangleCount;

        bool isLeaf = triangleCount > 0u;

        if (isLeaf)
        {
            for (uint i = 0u; i < triangleCount; i++)
            {
                // vec3 distData = rayTriangle(ray, node.TriangleStart + i);

                ////////////////////// ray-triangle intersection //////////////////////
                PackedTriangle triangle = triangles[triangleStart + i];

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

                bool hit = dist >= 0.0 && u >= 0.0 && v >= 0.0 && w >= 0.0;
                
                if (hit && dist < closestDistData.x)
                {
                    closestDistData = vec3(dist, u, v);
                    closestTriIndex = triangleStart + i;

                    frontFacing = dot(ray.direction, normal) < 0.0;
                }
            }
        }
        else
        {
            uint childIndexA = triangleStart;
            BVHNode childA = nodes[childIndexA];

            ////////////////////// ray-box intersection //////////////////////
            vec3 Min = childA.Min;
            vec3 Max = childA.Max;

            vec3 tMinA = (Min - ray.origin) * ray.invDirection;
            vec3 tMaxA = (Max - ray.origin) * ray.invDirection;

            vec3 t1A = min(tMinA, tMaxA);
            float tNearA = max(max(t1A.x, t1A.y), t1A.z);

            vec3 t2A = max(tMinA, tMaxA);
            float tFarA = min(min(t2A.x, t2A.y), t2A.z);

            float dstA = max(mix(1E7, tNearA, tFarA >= tNearA && tFarA > 0.0), 0.0);

            BVHNode childB = nodes[childIndexA + 1u];

            ////////////////////// ray-box intersection //////////////////////
            Min = childB.Min;
            Max = childB.Max;

            vec3 tMinB = (Min - ray.origin) * ray.invDirection;
            vec3 tMaxB = (Max - ray.origin) * ray.invDirection;

            vec3 t1B = min(tMinB, tMaxB);
            float tNearB = max(max(t1B.x, t1B.y), t1B.z);

            vec3 t2B = max(tMinB, tMaxB);
            float tFarB = min(min(t2B.x, t2B.y), t2B.z);

            float dstB = max(mix(1E7, tNearB, tFarB >= tNearB && tFarB > 0.0), 0.0);
            
            // We want to look at closest child node first, so push it last
            bool isNearestA = dstA <= dstB;
            float dstNear = isNearestA ? dstA : dstB;
            float dstFar = isNearestA ? dstB : dstA;
            uint childIndexNear = isNearestA ? childIndexA : childIndexA + 1u;
            uint childIndexFar = isNearestA ? childIndexA + 1u : childIndexA;

            if (dstFar < closestDistData.x) stack[stackIndex++] = childIndexFar;
            if (dstNear < closestDistData.x) stack[stackIndex++] = childIndexNear;
        }
    }

    return closestDistData;
}

// https://jcgt.org/published/0007/04/01/paper.pdf
vec3 sampleGGXVNDF(vec3 Ve, float alpha_x, float alpha_y, float U1, float U2) {
    // Section 3.2: transforming the view direction to the hemisphere configuration
    vec3 Vh = normalize(vec3(alpha_x * Ve.x, alpha_y * Ve.y, Ve.z));
    // Section 4.1: orthonormal basis (with special case if cross product is zero)
    float lensq = Vh.x * Vh.x + Vh.y * Vh.y;
    vec3 T1 = lensq > 0.0 ? vec3(-Vh.y, Vh.x, 0.0) * inversesqrt(lensq) : vec3(1.0, 0.0, 0.0);
    vec3 T2 = cross(Vh, T1);
    // Section 4.2: parameterization of the projected area
    float r = FastSqrt(U1);
    float phi = 2.0 * PI * U2;
    float t1 = r * cos(phi);
    float t2 = r * sin(phi);
    float s = 0.5 * (1.0 + Vh.z);
    t2 = (1.0 - s) * FastSqrt(1.0 - t1 * t1) + s * t2;
    // Section 4.3: reprojection onto hemisphere
    vec3 Nh = t1 * T1 + t2 * T2 + FastSqrt(max(0.0, 1.0 - t1 * t1 - t2 * t2)) * Vh;
    // Section 3.4: transforming the normal back to the ellipsoid configuration
    vec3 Ne = normalize(vec3(alpha_x * Nh.x, alpha_y * Nh.y, max(0.0, Nh.z)));
    return Ne;
}

vec3 calculateMicrofacetNormal(vec3 normal, float perceptualRoughness, vec3 rayDirection, inout uint rnd)
{
    rayDirection = -rayDirection;
    vec3 surfaceNormal = normal;

    float U1 = RandomValue(rnd);
    float U2 = RandomValue(rnd);

    vec3 tangent = normalize(abs(normal.z) < 0.999 ? cross(normal, vec3(0.0, 0.0, 1.0)) : cross(normal, vec3(0.0, 1.0, 0.0)));
    vec3 bitangent = cross(normal, tangent);

    vec3 tangentRayDirection = vec3(dot(rayDirection, tangent), dot(rayDirection, bitangent), dot(rayDirection, normal));

    float alpha = perceptualRoughness * perceptualRoughness;
    vec3 sampledNormal = sampleGGXVNDF(tangentRayDirection, alpha, alpha, U1, U2);

    // from tangent-space vector to world-space sample vector
    vec3 microfacetNormal = normalize(
        tangent * sampledNormal.x +
        bitangent * sampledNormal.y +
        normal * sampledNormal.z
    );

    return microfacetNormal;
}

float reflectance(float cosTheta, float ri) {
    float r0 = (1.0 - ri) / (1.0 + ri);
    r0 = r0 * r0;
    return r0 + (1.0 - r0) * pow(1.0 - cosTheta, 5.0);
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

    ray.direction = normalize(ray.direction);

    Ray rayData = Ray(ray.origin, ray.direction, 1.0 / ray.direction);

    bool frontFacing;
    vec3 distData = RayTriangleBVH(rayData, triangleIndex, frontFacing);

    if (distData.x >= 1E6) {
        ray.incomingLight += ray.color * sampleSkybox(rayData);
        ray.color *= 0.0; // ray has hit nothing, so it's done
        return;
    }

    // if (exp(-GlobalFogDensity * distData.x) < RandomValue(state))
    // {
    //     ray.color *= GlobalFogColor;

    //     distData.x *= RandomValue(state);
    //     ray.origin += ray.direction * distData.x;

    //     ray.direction = RandomDirection(state);

    //     return;
    // }

    float u = distData.y;
    float v = distData.z;
    float w = 1.0 - u - v;

    ray.origin += ray.direction * (distData.x - 0.0001);

    TriangleData triangle = triangleDatas[triangleIndex];
    uint materialIndex = triangle.material;

    vec3 normal = UnpackNormalInt32(triangle.normals.x) * w + UnpackNormalInt32(triangle.normals.y) * u + UnpackNormalInt32(triangle.normals.z) * v;
    vec2 uv = w * unpackHalf2x16(triangle.UVs.x) + u * unpackHalf2x16(triangle.UVs.y) + v * unpackHalf2x16(triangle.UVs.z);

    if (!frontFacing)
    {
        normal = -normal;
    }

    // Material data

    Material material = materials[materialIndex];

    ivec4 indices = UnpackInt16Vec4Int32vec2(material.materialIndices);

    vec4 albedo = unpackUnorm4x8(material.albedo.x);
    if (indices.x - 1 >= 0)
    {
        vec2 scale = albedoScales[indices.x - 1].scale;
        albedo *= texture(AlbedoTexture, vec3(uv * scale, indices.x - 1));
    }

    if (albedo.a <= 0.5 || length(albedo.rgb) == 0.0) // alpha test
    {
        ray.origin += ray.direction * 0.0004;
        return;
    }
    
    vec3 emissive = vec3(unpackHalf2x16(material.material.x), unpackHalf2x16(material.material.y).x);
    if (indices.y - 1 >= 0)
    {
        vec2 scale = emissiveScales[indices.y - 1].scale;
        emissive *= texture(EmissiveTexture, vec3(uv * scale, indices.y - 1)).rgb;
    }

    // material.material.y: 16emmisive.b, 8roughness unorm8, 8metallic unorm8
    float perceptualRoughness = (material.material.y >> 16) & 0xFF;
    float metallic = (material.material.y >> 24) & 0xFF;
    if (indices.w - 1 >= 0)
    {
        vec2 scale = metallicRoughnessScales[indices.w - 1].scale;
        vec2 data = texture(MetallicRoughnessTexture, vec3(uv * scale, indices.w - 1)).gb;
        perceptualRoughness = max(MIN_PERCEPTUAL_ROUGHNESS, data.x);
        metallic = data.y;
    }

    perceptualRoughness *= 0.0;

    // Material data

    ray.incomingLight += emissive * ray.color;

    vec3 microfacetNormal = calculateMicrofacetNormal(normal, perceptualRoughness, ray.direction, state);

    vec3 reflected = reflect(ray.direction, microfacetNormal);

    if (dot(-reflected, normal) < 0.0)
    {
        reflected = reflect(ray.direction, normal);
    }

    if (RandomValue(state) < albedo.a)
    {
        if (RandomValue(state) < metallic)
        {
            ray.direction = reflected;
        }
        else
        {
            ray.direction = RandomHemisphereDirection(normal, state);
        }

        ray.color *= albedo.rgb;
    }
    else
    {
        const float RefrectionIndex = 1.2;

        float cosTheta = abs(dot(-ray.direction, normal));
        float eta = frontFacing ? 1.0 / RefrectionIndex : RefrectionIndex;

        float sin2ThetaT = (1.0 - cosTheta * cosTheta) * eta * eta;

        bool canRefract = sin2ThetaT < 1.0;

        if (!canRefract || reflectance(cosTheta, eta) > RandomValue(state))
        {
            ray.direction = reflected;
        }
        else
        {
            ray.origin += ray.direction * 0.0004;
            float cosThetaT = sqrt(1.0 - sin2ThetaT);
            
            ray.direction = eta * ray.direction + (eta * cosTheta - cosThetaT) * normal;
        }
        
        ray.color *= albedo.rgb;
    }
    
}

uniform highp uint RandomIndex;

void computemain() {
    uint pixelIndex = gl_GlobalInvocationID.x;
    uint rngState = pixelIndex + RandomIndex;

    RayInfo rayInfo = rayInfos[pixelIndex];

    if (max(max(rayInfo.color.r, rayInfo.color.g), rayInfo.color.b) == 0.0)
    {
        return;
    }

    traceRay(rayInfo, rngState);

    rayInfos[pixelIndex] = rayInfo;
}