---@param vec vec3
function Rhodium.graphics.setCameraPosition(vec)
    Rhodium.internal.graphicsData.cameraPosition:set(vec.x, vec.y, vec.z)
end

---@return vec3
function Rhodium.graphics.getCameraPosition()
    return vec3(Rhodium.internal.graphicsData.cameraPosition)
end

---@param mat matrix4x4
function Rhodium.graphics.setCameraMatrix(mat)
    Rhodium.internal.graphicsData.cameraMatrix = mat:copy()
end

---@return matrix4x4
function Rhodium.graphics.getCameraMatrix()
    return Rhodium.internal.graphicsData.cameraMatrix
end

local function calculateSSRLodOffset(verticalFov, height)
    local texelSize1m = math.tan(verticalFov / 2) / height
    local kernelSize = 21
    local sigma0 = (kernelSize + 1) / 6
    local lodOffset = -math.log(texelSize1m * math.sqrt(2) * sigma0, 2)
    return lodOffset
end

function Rhodium.internal.calculateGaussianKernel(kernelSize)
    --[[
        float sigma = float(kernelSize) * 0.25;
        float[kernelSize * kernelSize] generateGaussianKernel(float sigma) {
            float[kernelSize * kernelSize] kernel;
            float sum = 0.0;
            for (int y = -kernelSize / 2; y <= kernelSize / 2; ++y) {
                for (int x = -kernelSize / 2; x <= kernelSize / 2; ++x) {
                    float value = (1.0 / (2.0 * 3.14159265359 * sigma * sigma)) * exp(-((float(x) * float(x) + float(y) * float(y)) / (2.0 * sigma * sigma)));
                    sum += value;
                    kernel[(y + 2) * kernelSize + (x + 2)] = value;
                }
            }
            for (int i = 0; i < kernelSize * kernelSize; ++i) {
                kernel[i] /= sum;
            }

            return kernel;
        }
        float[kernelSize * kernelSize] gaussianKernel = generateGaussianKernel(sigma);
    ]]
    local sigma = kernelSize * 0.25
    local kernel = {}
    local sum = 0
    for y = -kernelSize / 2, kernelSize / 2 do
        for x = -kernelSize / 2, kernelSize / 2 do
            local value = (1 / (2 * 3.14159265359 * sigma * sigma)) * math.exp(-((x * x + y * y) / (2 * sigma * sigma)))
            sum = sum + value
            kernel[(y + 2) * kernelSize + (x + 2)] = value
        end
    end
    for i = 1, kernelSize * kernelSize do
        kernel[i] = kernel[i] / sum
    end
    return kernel
end

---@param left number
---@param right number
---@param top number
---@param bottom number
---@param near number
---@param far number
function Rhodium.graphics.setCameraProjectionMatrix(left, right, top, bottom, near, far)
    local mat = Rhodium.graphics.newPerspectiveProjectionMatrix(left, right, top, bottom, near, far)

    local verticalFov = math.abs(2 * math.atan(1 / mat[2][2]))

    local lodOffset = calculateSSRLodOffset(verticalFov, love.graphics.getHeight())

    Rhodium.internal.graphicsData.cameraProjectionMatrix = mat:copy()
    Rhodium.internal.graphicsData.inverseCameraProjectionMatrix = mat:transpose():invert()

    --Rhodium.internal.shaders.main:send("CameraNear", near)
    --Rhodium.internal.shaders.main:send("CameraFar", far)
    Rhodium.internal.shaders.main:send("LodOffset", lodOffset)

    Rhodium.internal.shaders.transparencyShader:send("CameraNear", near)
    Rhodium.internal.shaders.transparencyShader:send("CameraFar", far)
    Rhodium.internal.shaders.transparencyShader:send("LodOffset", lodOffset)

    Rhodium.internal.shaders.screenSpaceReflections:send("CameraNear", near)
    Rhodium.internal.shaders.screenSpaceReflections:send("CameraFar", far)

    Rhodium.internal.shaders.ssao:send("CameraNear", near)
    Rhodium.internal.shaders.ssao:send("CameraFar", far)

    local graphicsData = Rhodium.internal.graphicsData

    graphicsData.far = far
    graphicsData.near = near
    graphicsData.left = left
    graphicsData.right = right
    graphicsData.top = top
    graphicsData.bottom = bottom
end

---@param mat matrix4x4
function Rhodium.graphics.setViewMatrix(mat)
    Rhodium.internal.graphicsData.viewMatrix = mat:copy()
end

---@return matrix4x4
function Rhodium.graphics.getViewMatrix()
    return Rhodium.internal.graphicsData.viewMatrix
end

---@return matrix4x4
function Rhodium.graphics.getCameraProjectionMatrix()
    return Rhodium.internal.graphicsData.cameraProjectionMatrix:copy()
end

---@param position vec3
---@param up vec3
---@param right vec3
---@param size number
function Rhodium.graphics.newPlane(position, up, right, size)
    local forward = up:cross(right):normalize()
    size = size / 2
    local vertices = {
        position + forward * size + right * size,
        position + forward * size - right * size,
        position - forward * size - right * size,
        position - forward * size + right * size,
    }
    local vertices = {
        { vertices[1].x, vertices[1].y, vertices[1].z, 0, 0, up.x, up.y, up.z, right.x, right.y, right.z },
        { vertices[2].x, vertices[2].y, vertices[2].z, 0, 1, up.x, up.y, up.z, right.x, right.y, right.z },
        { vertices[3].x, vertices[3].y, vertices[3].z, 1, 1, up.x, up.y, up.z, right.x, right.y, right.z },
        { vertices[4].x, vertices[4].y, vertices[4].z, 1, 0, up.x, up.y, up.z, right.x, right.y, right.z },
    }
    local indices = {
        1, 2, 3, 1, 3, 4,
    }
    return Rhodium.math.verticesToTriangles(vertices, indices)
end

function Rhodium.graphics.newOrthographicProjectionMatrix(left, right, top, bottom, near, far)
    return mat4(
        2 / (right - left), 0, 0, -(right + left) / (right - left),
        0, 2 / (top - bottom), 0, -(top + bottom) / (top - bottom),
        0, 0, -2 / (far - near), -(far + near) / (far - near),
        0, 0, 0, 1
    )
end

function Rhodium.graphics.newPerspectiveProjectionMatrix(left, right, top, bottom, near, far)
    return mat4((near * 2) / (right - left), 0, (right + left) / (right - left), 0,
        0, (near * 2) / (top - bottom), (top + bottom) / (top - bottom), 0,
        0, 0, -((far + near) / (far - near)), -(2 * far * near) / (far - near),
        0, 0, -1, 0
    )
end

-- https://google.github.io/filament/Filament.html#sphericalharmonics
local function SHindex(m, l)
    return l * (l + 1) + m
end

--- Computes the spherical harmonics basis for the given direction
---@param SHb table the spherical harmonics basis fills this table
---@param numBands number the number of bands to compute
---@param s vec3 the direction to compute the spherical harmonics basis for
local function computeShBasis(SHb, numBands, s)
    local Pml_2 = 0
    local Pml_1 = 1
    SHb[SHindex(0, 0)] = Pml_1
    for l = 1, numBands - 1 do
        local Pml = ((2 * l - 1) * Pml_1 * s[3] - (l - 1) * Pml_2) / l
        Pml_2 = Pml_1
        Pml_1 = Pml
        SHb[SHindex(0, l)] = Pml
    end
    local Pmm = 1
    for m = 1, numBands - 1 do
        Pmm = (1 - 2 * m) * Pmm
        local Pml_2 = Pmm
        local Pml_1 = (2 * m + 1) * Pmm * s[3]
        SHb[SHindex(-m, m)] = Pml_2
        SHb[SHindex(m, m)] = Pml_2
        if m + 1 < numBands then
            SHb[SHindex(-m, m + 1)] = Pml_1
            SHb[SHindex(m, m + 1)] = Pml_1
            for l = m + 2, numBands - 1 do
                local Pml = ((2 * l - 1) * Pml_1 * s[3] - (l + m - 1) * Pml_2) / (l - m)
                Pml_2 = Pml_1
                Pml_1 = Pml
                SHb[SHindex(-m, l)] = Pml
                SHb[SHindex(m, l)] = Pml
            end
        end
    end
    local Cm = s[1]
    local Sm = s[2]
    for m = 1, numBands - 1 do
        for l = m, numBands - 1 do
            SHb[SHindex(-m, l)] = SHb[SHindex(-m, l)] * Sm
            SHb[SHindex(m, l)] = SHb[SHindex(m, l)] * Cm
        end
        local Cm1 = Cm * s[1] - Sm * s[2]
        local Sm1 = Sm * s[1] + Cm * s[2]
        Cm = Cm1
        Sm = Sm1
    end
end

--[[
float GDFG(float NoV, float NoL, float a) {
    float a2 = a * a;
    float GGXL = NoV * sqrt((-NoL * a2 + NoL) * NoL + a2);
    float GGXV = NoL * sqrt((-NoV * a2 + NoV) * NoV + a2);
    return (2 * NoL) / (GGXV + GGXL);
}

float2 DFG(float NoV, float a) {
    float3 V;
    V.x = sqrt(1.0f - NoV*NoV);
    V.y = 0.0f;
    V.z = NoV;

    float2 r = 0.0f;
    for (uint i = 0; i < sampleCount; i++) {
        float2 Xi = hammersley(i, sampleCount);
        float3 H = importanceSampleGGX(Xi, a, N);
        float3 L = 2.0f * dot(V, H) * H - V;

        float VoH = saturate(dot(V, H));
        float NoL = saturate(L.z);
        float NoH = saturate(H.z);

        if (NoL > 0.0f) {
            float G = GDFG(NoV, NoL, a);
            float Gv = G * VoH / NoH;
            float Fc = pow(1 - VoH, 5.0f);
            r.x += Gv * (1 - Fc);
            r.y += Gv * Fc;
        }
    }
    return r * (1.0f / sampleCount);
}
vec2f hammersley(uint i, float numSamples) {
    uint bits = i;
    bits = (bits << 16) | (bits >> 16);
    bits = ((bits & 0x55555555) << 1) | ((bits & 0xAAAAAAAA) >> 1);
    bits = ((bits & 0x33333333) << 2) | ((bits & 0xCCCCCCCC) >> 2);
    bits = ((bits & 0x0F0F0F0F) << 4) | ((bits & 0xF0F0F0F0) >> 4);
    bits = ((bits & 0x00FF00FF) << 8) | ((bits & 0xFF00FF00) >> 8);
    return vec2f(i / numSamples, bits / exp2(32));
}
]]

local bit = require("bit")

local function exp2(x)
    return 2 ^ x
end

local invEXP32 = 1 / exp2(32)

local function hammersley(i, invNumSamples)
    local bits = i

    bits = bit.bor(bit.lshift(bits, 16), bit.rshift(bits, 16))
    bits = bit.bor(bit.lshift(bit.band(bits, 0x55555555), 1), bit.rshift(bit.band(bits, 0xAAAAAAAA), 1))
    bits = bit.bor(bit.lshift(bit.band(bits, 0x33333333), 2), bit.rshift(bit.band(bits, 0xCCCCCCCC), 2))
    bits = bit.bor(bit.lshift(bit.band(bits, 0x0F0F0F0F), 4), bit.rshift(bit.band(bits, 0xF0F0F0F0), 4))
    bits = bit.bor(bit.lshift(bit.band(bits, 0x00FF00FF), 8), bit.rshift(bit.band(bits, 0xFF00FF00), 8))
    return i * invNumSamples, bits * invEXP32
end

local function importanceSampleGGX(Xi, a, N)
    local Phi = 2 * math.pi * Xi[1]
    local CosTheta = math.sqrt((1 - Xi[2]) / (1 + (a * a - 1) * Xi[2]))
    local SinTheta = math.sqrt(1 - CosTheta * CosTheta)
    local H = vec3(SinTheta * math.cos(Phi), SinTheta * math.sin(Phi), CosTheta)
    local UpVector = math.abs(N[3]) < 0.999 and vec3(0, 0, 1) or vec3(1, 0, 0)
    local TangentX = UpVector:cross(N):normalize()
    local TangentY = N:cross(TangentX)
    return TangentX * H[1] + TangentY * H[2] + N * H[3]
end

--[[
    static float3 hemisphereImportanceSampleDggx(float2 u, float a) { // pdf = D(a) * cosTheta
    const float phi = 2.0f * (float) F_PI * u.x;
    // NOTE: (aa-1) == (a-1)(a+1) produces better fp accuracy
    const float cosTheta2 = (1 - u.y) / (1 + (a + 1) * ((a - 1) * u.y));
    const float cosTheta = std::sqrt(cosTheta2);
    const float sinTheta = std::sqrt(1 - cosTheta2);
    return { sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta };
}
]]

local function hemisphereImportanceSampleDggx(u, a)
    local phi = 2.0 * math.pi * u.x
    local cosTheta2 = (1 - u.y) / (1 + (a + 1) * ((a - 1) * u.y))
    local cosTheta = math.sqrt(cosTheta2)
    local sinTheta = math.sqrt(1 - cosTheta2)
    return sinTheta * math.cos(phi), sinTheta * math.sin(phi), cosTheta
end

local function Visibility(NoV, NoL, a)
    -- Heitz 2014, "Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs"
    -- Height-correlated GGX
    local a2 = a * a
    local GGXL = NoV * math.sqrt((NoL - NoL * a2) * NoL + a2)
    local GGXV = NoL * math.sqrt((NoV - NoV * a2) * NoV + a2)
    return 0.5 / (GGXV + GGXL)
end

local function pow5(x)
    local x2 = x * x
    return x2 * x2 * x
end

function Rhodium.internal.DFV(NoV, linearRoughness, sampleCount)
    local V = vec3(math.sqrt(1.0 - NoV * NoV), 0.0, NoV)

    local rx, ry = 0.0, 0.0

    local invSampleCount = 1.0 / sampleCount

    local VHH = vec3()
    local L = vec3()
    local H = vec3()
    local u = vec2()

    for i = 0, sampleCount - 1 do
        u:set(hammersley(i, invSampleCount))
        H:set(hemisphereImportanceSampleDggx(u, linearRoughness))
        local VH = V:dot(H)
        VHH:set(VH * H)
        L:set(2.0 * VHH - V)

        local VoH = Rhodium.math.saturate(VH)
        local NoL = Rhodium.math.saturate(L.z)
        local NoH = Rhodium.math.saturate(H.z)

        if NoL > 0 then
            local v = Visibility(NoV, NoL, linearRoughness) * NoL * (VoH / NoH)
            local Fc = pow5(1.0 - VoH)
            rx = rx + v * (1.0 - Fc)
            ry = ry + v * Fc
        end
    end

    H:release()
    L:release()
    u:release()
    VHH:release()
    V:release()
    local sample = (4.0 / sampleCount)
    return rx * sample, ry * sample
end

---@param vertexformat table
---@param vertices table
---@param mode? love.MeshDrawMode
---@param usage? love.SpriteBatchUsage
---@param calculateTangents? boolean
---@param calculateNormals? boolean
---@return love.Mesh
function Rhodium.graphics.newMesh(vertexformat, vertices, mode, usage, calculateTangents, calculateNormals)
    mode = mode or "triangles"
    if mode == "triangles" then
        for i = 1, #vertices, 3 do
            local p1, p2, p3 = vertices[i], vertices[i + 1], vertices[i + 2]
            if calculateTangents or calculateNormals then -- the user might send a mesh that is intended to be used with a vertexmap
                assert(p1 and p2 and p3, "invalid mesh, not enough vertices")
                assert(p1[1] and p2[1] and p3[1], "invalid mesh, missing position")
            end
            if calculateTangents ~= false then -- the user might send a mesh that already has tangents and normals
                assert(p1[4] and p2[4] and p3[4], "invalid mesh, missing uv")
                local tangentX, tangentY, tangentZ = Rhodium.math.triangleTangent(p1, p2, p3)
                vertices[i][9], vertices[i][10], vertices[i][11] = tangentX, tangentY, tangentZ
                vertices[i + 1][9], vertices[i + 1][10], vertices[i + 1][11] = tangentX, tangentY, tangentZ
                vertices[i + 2][9], vertices[i + 2][10], vertices[i + 2][11] = tangentX, tangentY, tangentZ
            end
            if calculateNormals ~= false then -- the user might send a mesh that already has tangents and normals
                local x, y, z = Rhodium.math.triangleNormal(p1, p2, p3)
                vertices[i][6], vertices[i][7], vertices[i][8] = x, y, z
                vertices[i + 1][6], vertices[i + 1][7], vertices[i + 1][8] = x, y, z
                vertices[i + 2][6], vertices[i + 2][7], vertices[i + 2][8] = x, y, z
            end
        end
    end
    return love.graphics.newMesh(Rhodium.internal.vertexformat, vertices, mode, usage)
end
