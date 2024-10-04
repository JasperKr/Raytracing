#pragma language glsl4

#define SKIP_VIEW_Z 1
#define SKIP_GET_POSITION_DATA 1

#include "functions.glsl"

const int kernelSize = 5; // Adjust the size as needed

#define PI 3.1415926535897932384626433832795
#define saturate(x)        clamp(x, 0.0, 1.0)

#define MIN_PERCEPTUAL_ROUGHNESS 0.045
#define MIN_ROUGHNESS 0.002025

#define MEDIUMP_FLT_MAX    65504.0
#define saturateMediump(x) min(x, MEDIUMP_FLT_MAX)

varying vec2 VarVertexCoord;
uniform bool FlipY;
#ifdef VERTEX
void vertexmain() {
    VarVertexCoord = vec2((love_VertexID << 1) & 2, love_VertexID & 2);

    vec4 VarScreenPosition = vec4(VarVertexCoord.xy * vec2(2.0, -2.0) + vec2(-1.0, 1.0), 0, 1);

    // OpenGL Flip
    if (!FlipY)
        VarVertexCoord.y = 1.0 - VarVertexCoord.y;

    gl_Position = VarScreenPosition;
}
#endif

struct TextureInfo {
    vec2 scale;
};

struct Material {
    vec3 albedo;
    vec3 emissive;
    float perceptualRoughness;
    float metallic;
    int albedoIndex;
    int emissiveIndex;
    int normalIndex;
    int materialIndex;
};

struct Triangle {
    vec4 A; // Position and u
    vec4 B; // Position and u
    vec4 C; // Position and u
    vec3 V; // v for a, b, c
    uint material;
    vec3 normal;
};

struct BVHNode {
    vec3 Min;
    vec3 Max;
    uint TriangleStart;
    uint TriangleCount;
};

readonly restrict buffer Triangles {
    Triangle triangles[];
};

readonly restrict buffer Materials {
    Material materials[];
};

readonly restrict buffer BVHNodes {
    BVHNode nodes[];
};

/// Textures ///
uniform lowp sampler2DArray AlbedoTexture;
uniform lowp sampler2DArray EmissiveTexture;
uniform lowp sampler2DArray NormalTexture;
uniform lowp sampler2DArray MetallicRoughnessTexture;

readonly restrict buffer AlbedoBuffer {
    TextureInfo albedoScales[];
};

readonly restrict buffer EmissiveBuffer {
    TextureInfo emissiveScales[];
};

readonly restrict buffer NormalBuffer {
    TextureInfo normalScales[];
};

readonly restrict buffer MetallicRoughnessBuffer {
    TextureInfo metallicRoughnessScales[];
};

/// Textures ///

struct Ray {
    vec3 origin;
    vec3 direction;
    vec3 invDirection;
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

float rayBoxDistance(vec3 bmin, vec3 bmax, Ray ray) {
    vec3 tMin = (bmin - ray.origin) * ray.invDirection;
    vec3 tMax = (bmax - ray.origin) * ray.invDirection;

    vec3 t1 = min(tMin, tMax);
    float tNear = max(max(t1.x, t1.y), t1.z);

    vec3 t2 = max(tMin, tMax);
    float tFar = min(min(t2.x, t2.y), t2.z);

    return max(mix(1E7, tNear, tFar >= tNear && tFar > 0.0), 0.0);
}

vec3[3] getMaterialData(uint materialIndex, vec2 uv)
{
    Material material = materials[materialIndex];

    vec3 albedo = material.albedo;
    if (material.albedoIndex >= 0)
    {
        vec2 scale = albedoScales[material.albedoIndex].scale;
        albedo = texture(AlbedoTexture, vec3(uv * scale, material.albedoIndex)).rgb;
    }
    
    vec3 emissive = material.emissive;
    if (material.emissiveIndex >= 0)
    {
        vec2 scale = emissiveScales[material.emissiveIndex].scale;
        emissive *= texture(EmissiveTexture, vec3(uv * scale, material.emissiveIndex)).rgb;
    }
    
    float perceptualRoughness = material.perceptualRoughness;
    float metallic = material.metallic;
    if (material.materialIndex >= 0)
    {
        vec2 scale = metallicRoughnessScales[material.materialIndex].scale;
        vec2 data = texture(MetallicRoughnessTexture, vec3(uv * scale, material.materialIndex)).gb;
        perceptualRoughness = max(MIN_PERCEPTUAL_ROUGHNESS, data.x);
        metallic = data.y;
    }

    return vec3[3](albedo, emissive, vec3(perceptualRoughness, metallic, 0.0));
}

vec3 rayTriangle(Ray ray, uint triIndex) {
    Triangle triangle = triangles[triIndex];

    vec3 bmin = min(min(triangle.A.xyz, triangle.B.xyz), triangle.C.xyz);
    vec3 bmax = max(max(triangle.A.xyz, triangle.B.xyz), triangle.C.xyz);

    bmin -= 0.01;
    bmax += 0.01;

    if (rayBoxDistance(bmin, bmax, ray) < 1E6)
    {
        vec3 AB = triangle.B.xyz - triangle.A.xyz;
        vec3 AC = triangle.C.xyz - triangle.A.xyz;
        vec3 normal = cross(AB, AC);
        vec3 AO = ray.origin - triangle.A.xyz;
        vec3 DAO = cross(AO, ray.direction);

        float det = -dot(ray.direction, normal);
        float invDet = 1.0 / det;

        float dist = dot(AO, normal) * invDet;
        float u = dot(AC, DAO) * invDet;
        float v = -dot(AB, DAO) * invDet;
        float w = 1.0 - u - v;

        float hit = float(det > 1E-6 && dist >= 0.0 && u >= 0.0 && v >= 0.0 && w >= 0.0);
        
        return vec3(mix(-1.0, dist, hit), u, v);
    }

    return vec3(-1.0);
}

vec3 RayTriangleBVH(Ray ray, inout ivec2 amountChecked, out uint closestTriIndex)
{
    const int stackSize = 32;

    uint stack[stackSize];
    int stackIndex = 0;
    stack[stackIndex++] = 0u;
    vec3 closestDistData = vec3(1E6);
    vec2 closestUV = vec2(0.0);

    closestTriIndex = 0u;

    int max = 1000;

    while (stackIndex > 0 && max--> 0 && stackIndex < stackSize)
    {
        BVHNode node = nodes[stack[--stackIndex]];
        bool isLeaf = node.TriangleCount > 0u;

        if (isLeaf)
        {
            for (uint i = 0u; i < node.TriangleCount; i++)
            {
                vec3 distData = rayTriangle(ray, node.TriangleStart + i);
                amountChecked.y++; // count triangle intersection tests

                if (distData.x > 0.0 && distData.x < closestDistData.x)
                {
                    closestDistData = distData;
                    closestTriIndex = node.TriangleStart + i;
                }
            }
        }
        else
        {
            uint childIndexA = node.TriangleStart + 0u;
            uint childIndexB = node.TriangleStart + 1u;
            BVHNode childA = nodes[childIndexA];
            float dstA = rayBoxDistance(childA.Min, childA.Max, ray);

            BVHNode childB = nodes[childIndexB];
            float dstB = rayBoxDistance(childB.Min, childB.Max, ray);
            amountChecked.x += 2; // count bounding box intersection tests
            
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

vec3 checkRayIntersections(Ray ray, out uint closestTriIndex, inout ivec2 amountChecked)
{
    // this function will be filled in later once i have separate models
    vec3 closestDistData = RayTriangleBVH(ray, amountChecked, closestTriIndex);

    return closestDistData;
}

uniform mediump samplerCube Skybox;
uniform highp vec3 CameraPosition;
uniform highp mat4 InverseViewProjectionMatrix;

const float skyboxBrightness = 0.0;

vec3 sampleSkybox(Ray ray)
{
    return texture(Skybox, ray.direction).rgb * skyboxBrightness;
}

const int MaxBounces = 6;
const float GlobalFogDensity = 0.2;
const vec3 GlobalFogColor = vec3(0.8, 0.8, 0.8);

vec3 traceRay(Ray ray, inout uint state, inout ivec2 amountChecked, out float depth) {
    vec3 rayColor = vec3(1.0);
    vec3 rayIncomingLight = vec3(0.0);

    for (int bounceIndex = 0; bounceIndex < MaxBounces; bounceIndex++)
    {
        uint triangleIndex;

        vec3 distData = checkRayIntersections(ray, triangleIndex, amountChecked);

        if (distData.x >= 1E6)
            return rayIncomingLight + rayColor * sampleSkybox(ray);

        if (bounceIndex == 0)
            depth = distData.x;

        if (exp(-GlobalFogDensity * distData.x) < RandomValue(state))
        {
            rayColor *= GlobalFogColor;

            distData.x *= RandomValue(state);
            ray.origin += ray.direction * distData.x;

            ray.direction = RandomDirection(state);
            ray.invDirection = 1.0 / ray.direction;

            continue;
        }

        float u = distData.y;
        float v = distData.z;
        float w = 1.0 - u - v;

        ray.origin += ray.direction * (distData.x + 0.0001);

        Triangle triangle = triangles[triangleIndex];
        uint materialIndex = triangle.material;
        vec3 normal = triangle.normal;

        vec2 finalUV = vec2(
            w * triangle.A.w + u * triangle.B.w + v * triangle.C.w,
            w * triangle.V.x + u * triangle.V.y + v * triangle.V.z
        );

        vec3[3] materialData = getMaterialData(materialIndex, finalUV);

        vec3 albedo = materialData[0];
        vec3 emissive = materialData[1];
        float perceptualRoughness = materialData[2].x;
        float metallic = materialData[2].y;

        rayIncomingLight += emissive * rayColor;

        float roughness = perceptualRoughness * perceptualRoughness;
        float isDiffuseHit = float(RandomValue(state) > metallic);

        if (isDiffuseHit > 0.0)
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

        // rayColor *= mix(vec3(1.0), albedo, isDiffuseHit);
        rayColor *= albedo;
        ray.invDirection = 1.0 / ray.direction;
    }

    return rayIncomingLight;
}

uniform highp uint RandomIndex;
uniform highp sampler2D PreviousFrame;
uniform highp uint FrameIndex;

uniform bool DebugView;
uniform mediump int DebugViewMode;

uniform highp mat4 PreviousViewProjectionMatrix;

vec3 reproject(vec3 position) {
    position = position * 2.0 - 1.0;
    vec4 world = mulVec3Matrix4x4(InverseViewProjectionMatrix, position);
    world.xyz /= world.w;

    vec4 clip = mulVec3Matrix4x4(PreviousViewProjectionMatrix, world.xyz);
    clip.xyz /= clip.w;
    return clip.xyz * 0.5 + 0.5;
}

#ifdef PIXEL

out vec4 FragColor;

void pixelmain() {
    vec4 world = mulVec3Matrix4x4(InverseViewProjectionMatrix, vec3(VarVertexCoord * 2.0 - 1.0, 1.0));

    vec3 rayDirection = normalize(world.xyz / world.w - CameraPosition);

    vec2 screen_coords = VarVertexCoord * love_ScreenSize.xy;

    uint pixelIndex = uint(screen_coords.y * love_ScreenSize.x + screen_coords.x);
    uint rngState = pixelIndex + RandomIndex;

    vec3 randomDirection = RandomHemisphereDirection(rayDirection, rngState);

    const float jitterAmount = 0.001;

    rayDirection = normalize(mix(rayDirection, randomDirection, jitterAmount));

    Ray ray = Ray(CameraPosition, rayDirection, 1.0 / rayDirection);

    ivec2 amountChecked = ivec2(0, 0);

    float depth;
    vec3 color = traceRay(ray, rngState, amountChecked, depth);

    world.xyz = rayDirection * depth + CameraPosition;

    // vec3 reprojected = reproject(world.xyz);
    vec3 reprojected = vec3(VarVertexCoord, 0.0);

    if (DebugView)
    {   
        if (DebugViewMode == 0) // Bounding box tests
            if (amountChecked.x > 1000)
                FragColor = vec4(1.0, 0.0, 0.0, 1.0);
            else
                FragColor = vec4(vec3(float(amountChecked.x) / 1000.0), 1.0);
        else if (DebugViewMode == 1) // Triangle tests
            if (amountChecked.y > 1000)
                FragColor = vec4(1.0, 0.0, 0.0, 1.0);
            else
                FragColor = vec4(vec3(float(amountChecked.y) / 1000.0), 1.0);
        else
            FragColor = vec4(color, 1.0);
        return;
    }

    highp float contribution = 1.0 / float(FrameIndex + 1u);

    vec3 previousColor = texture(PreviousFrame, reprojected.xy).rgb;

    FragColor = vec4(mix(previousColor, color, contribution), 1.0);
}

#endif