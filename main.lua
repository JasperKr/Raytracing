function love.load()
    ffi = require("ffi")
    Rhodium = {
        math = {},
        graphics = {},
        internal = {},
    }

    table.new = require("table.new")
    table.clear = require("table.clear")

    require("matrices")
    require("vec")
    require("Rmath")
    require("quaternions")
    require("graphicsFunctions")
    require("graphics")
    require("buffers")
    require("models")
    require("bvh")
    require("texturehandler")
    require("tables")
    require("shaders")
    Camera = {
        position = vec3(0, 0, 0),
        rotationMatrix = mat4(),
        viewMatrix = mat4(),
        viewProjectionMatrix = mat4(),
        pitch = 0,
        yaw = 0,
        roll = 0,
        speed = 2,
    }
    local aspect = love.graphics.getHeight() / love.graphics.getWidth()
    local fov = (120) / 720;
    Camera.projectionMatrix = Rhodium.graphics.newPerspectiveProjectionMatrix(-fov, fov, -fov * aspect, fov * aspect, 0.1,
        1000)

    Camera.inverseProjectionMatrix = Camera.projectionMatrix:invert():transpose()
    Camera.viewProjectionMatrix = Camera.projectionMatrix * Camera.viewMatrix
    Camera.inverseViewProjectionMatrix = Camera.viewProjectionMatrix:invert():transpose()
    Camera.inverseViewMatrix = Camera.viewMatrix:invert():transpose()

    Camera.previousProjectionMatrix = Camera.projectionMatrix
    Camera.previousViewMatrix = Camera.viewMatrix
    Camera.previousViewProjectionMatrix = Camera.viewProjectionMatrix
    Camera.previousInverseViewProjectionMatrix = Camera.inverseViewProjectionMatrix
    Camera.previousInverseViewMatrix = Camera.inverseViewMatrix

    MAX_DEPTH = 22

    Shaders = {
        rayTrace = Rhodium.graphics.newComputeShader("rayTrace.glsl"),
        rayInit = Rhodium.graphics.newComputeShader("rayInit.glsl"),
        rayWrite = Rhodium.graphics.newComputeShader("rayWrite.glsl"),
    }

    Skybox = love.graphics.newCubeImage("skybox.exr", { linear = true })

    Shaders.rayTrace:send("Skybox", Skybox)

    love.mouse.setRelativeMode(true)

    Target = love.graphics.newTexture(love.graphics.getWidth(), love.graphics.getHeight(),
        { format = "rgba32f", computewrite = true, canvas = true })

    local materialformat = {
        { name = "albedo",              format = "floatvec3" },
        { name = "emissve",             format = "floatvec3" },
        { name = "perceptualRoughness", format = "float" },
        { name = "metallic",            format = "float" },
        { name = "albedoIndex",         format = "int32" },
        { name = "emissiveIndex",       format = "int32" },
        { name = "normalIndex",         format = "int32" },
        { name = "materialIndex",       format = "int32" },
    }

    local triangleFormat = {
        { name = "posAU",    format = "floatvec4" },
        { name = "posBU",    format = "floatvec4" },
        { name = "posCU",    format = "floatvec4" },
        { name = "V",        format = "floatvec3" },
        { name = "material", format = "uint32" },
        { name = "normal",   format = "floatvec3" },
    }

    -- local meshes = Rhodium.graphics.loadGltfFile("Sponza/sponza.gltf")
    local meshes = Rhodium.graphics.loadGltfFile("sponza_with_light.glb")

    local triangleBuffer = newBuffer(triangleFormat, 1, { shaderstorage = true, usage = "static" })
    local triangles = {}

    local triangleCount = 0

    scale = 1

    local width, height = love.graphics.getDimensions()
    local rayCount = width * height

    local rayBufferFormat = {
        { name = "origin",        format = "floatvec3" },
        { name = "direction",     format = "floatvec3" },
        { name = "color",         format = "floatvec3" },
        { name = "incomingLight", format = "floatvec3" },
    }

    RayBuffer = newBuffer(rayBufferFormat, rayCount, { shaderstorage = true, usage = "dynamic" })

    local albedoTextures = {}
    local normalTextures = {}
    local emissiveTextures = {}
    local metallicRoughnessTextures = {}

    for j, mesh in ipairs(meshes) do
        ---@type love.ByteData, love.ByteData
        local meshVertices, meshIndices = mesh.vertices, mesh.indices

        local vertexCount = meshVertices:getSize() / ffi.sizeof(mesh.ffiVertexFormat)
        local indexFormat = mesh.CIndicesType .. "_t"
        local indexCount = meshIndices:getSize() / ffi.sizeof(indexFormat)

        local vertices, indices = {}, {}

        local ffiVertices = ffi.cast(mesh.ffiVertexFormat .. "*", meshVertices:getFFIPointer())
        local ffiIndices = ffi.cast(indexFormat .. "*", meshIndices:getFFIPointer())

        for i = 1, vertexCount do
            local vertex = ffiVertices[i - 1]
            local position = vertex.VertexPosition
            local normal = vertex.VertexNormal
            local uv = vertex.VertexTexCoord_0

            local x, y, z = rotatePositionSeparate(tonumber(position.x), tonumber(position.y), tonumber(position.z),
                mesh.rotation.x, mesh.rotation.y, mesh.rotation.z, mesh.rotation.w)

            local nx, ny, nz = rotatePositionSeparate(tonumber(normal.x), tonumber(normal.y), tonumber(normal.z),
                mesh.rotation.x, mesh.rotation.y, mesh.rotation.z, mesh.rotation.w)

            vertices[i] = {
                x * mesh.scale.x * scale + mesh.position.x,
                y * mesh.scale.y * scale + mesh.position.y,
                z * mesh.scale.z * scale + mesh.position.z,
                uv.x,
                uv.y,
                nx,
                ny,
                nz
            }
        end

        for i = 1, indexCount do
            indices[i] = tonumber(ffiIndices[i - 1]) + 1
        end

        triangleCount = triangleCount + indexCount / 3

        for i = 1, indexCount / 3 do
            local a = vertices[indices[(i - 1) * 3 + 1]]
            local b = vertices[indices[(i - 1) * 3 + 2]]
            local c = vertices[indices[(i - 1) * 3 + 3]]

            table.insert(triangles, {
                a[1], a[2], a[3], a[4],
                b[1], b[2], b[3], b[4],
                c[1], c[2], c[3], c[4],
                a[5], b[5], c[5],
                j - 1,
                a[6], a[7], a[8],
            })
        end
    end

    local materialsBuffer = newBuffer(materialformat, #meshes, { shaderstorage = true })

    for i = 1, #meshes do
        local mesh = meshes[i]
        local material = mesh.material

        local uniforms = material.uniforms

        uniforms.baseColor = uniforms.baseColor or { love.math.random(), love.math.random(), love.math.random() }
        uniforms.emissive = uniforms.emissive or { 0, 0, 0 }

        local albedoIndex = -1
        local emissiveIndex = -1
        local normalIndex = -1
        local materialIndex = -1

        if mesh.material.main_texture and mesh.material.main_texture.source then
            albedoIndex = #albedoTextures
            table.insert(albedoTextures, mesh.material.main_texture.source)
        end

        if mesh.material.uniforms.normalTexture then
            normalIndex = #normalTextures
            table.insert(normalTextures, mesh.material.uniforms.normalTexture)
        end

        if mesh.material.uniforms.emissiveTexture then
            emissiveIndex = #emissiveTextures
            table.insert(emissiveTextures, mesh.material.uniforms.emissiveTexture)
        end

        if mesh.material.uniforms.metallicRoughnessTexture then
            materialIndex = #metallicRoughnessTextures
            table.insert(metallicRoughnessTextures, mesh.material.uniforms.metallicRoughnessTexture)
        end

        materialsBuffer:write({
            { uniforms.baseColor[1],      uniforms.baseColor[2],      uniforms.baseColor[3] },
            { uniforms.emissiveFactor[1], uniforms.emissiveFactor[2], uniforms.emissiveFactor[3] },
            uniforms.roughness or 0.5,
            uniforms.metalness or 0.5,
            albedoIndex,
            emissiveIndex,
            normalIndex,
            materialIndex,
        })
    end

    local albedoTexture, albedoBuffer
    local normalTexture, normalBuffer
    local emissiveTexture, emissiveBuffer
    local metallicRoughnessTexture, metallicRoughnessBuffer

    if #albedoTextures > 0 then
        albedoTexture, albedoBuffer = newTextureGroup(albedoTextures, { format = "srgba8", mipmaps = "manual" })
    end
    if #normalTextures > 0 then
        normalTexture, normalBuffer = newTextureGroup(normalTextures, { format = "rgba8" })
    end
    if #emissiveTextures > 0 then
        emissiveTexture, emissiveBuffer = newTextureGroup(emissiveTextures, { format = "rgba8" })
    end
    if #metallicRoughnessTextures > 0 then
        metallicRoughnessTexture, metallicRoughnessBuffer = newTextureGroup(metallicRoughnessTextures,
            { format = "rgba8" })
    end


    -- materialsBuffer:write({ { 0.75, 0.75, 0.75 }, { 0, 0, 0 }, 1.0, 0.0 })

    materialsBuffer:flush()

    local node = newBvhTree(triangles)
    node.triangleCount = triangleCount

    sendBVHData()

    triangleBuffer:resize(triangleCount)
    triangleBuffer:write(triangles)
    triangleBuffer:flush()

    Shaders.rayTrace:send("Triangles", triangleBuffer:getBuffer())
    Shaders.rayTrace:send("Materials", materialsBuffer:getBuffer())
    Shaders.rayTrace:send("BVHNodes", BvhBuffer:getBuffer())

    if albedoTexture and Shaders.rayTrace:hasUniform("AlbedoTexture") then
        Shaders.rayTrace:send("AlbedoTexture", albedoTexture)
        Shaders.rayTrace:send("AlbedoBuffer", albedoBuffer:getBuffer())
    end

    if normalTexture and Shaders.rayTrace:hasUniform("NormalTexture") then
        Shaders.rayTrace:send("NormalTexture", normalTexture)
        Shaders.rayTrace:send("NormalBuffer", normalBuffer:getBuffer())
    end

    if emissiveTexture and Shaders.rayTrace:hasUniform("EmissiveTexture") then
        Shaders.rayTrace:send("EmissiveTexture", emissiveTexture)
        Shaders.rayTrace:send("EmissiveBuffer", emissiveBuffer:getBuffer())
    end

    if metallicRoughnessTexture and Shaders.rayTrace:hasUniform("MetallicRoughnessTexture") then
        Shaders.rayTrace:send("MetallicRoughnessTexture", metallicRoughnessTexture)
        Shaders.rayTrace:send("MetallicRoughnessBuffer", metallicRoughnessBuffer:getBuffer())
    end

    Shaders.rayInit:send("RayInfoBuffer", RayBuffer:getBuffer())
    Shaders.rayTrace:send("RayInfoBuffer", RayBuffer:getBuffer())
    Shaders.rayWrite:send("RayInfoBuffer", RayBuffer:getBuffer())

    MAX_BOUNCES = 4
end

function rotatePositionSeparate(x, y, z, qx, qy, qz, qw)
    local cx = qy * z - qz * y + x * qw
    local cy = qz * x - qx * z + y * qw
    local cz = qx * y - qy * x + z * qw

    return x + 2 * (qy * cz - qz * cy),
        y + 2 * (qz * cx - qx * cz),
        z + 2 * (qx * cy - qy * cx)
end

function getDataType(size)
    if size < 2 then
        return 'uint8'
    elseif size == 2 then
        return 'uint16'
    else
        return 'uint32'
    end
end

local timesFFIVertexFormatCreated = 0
local formats = {}

function vertexformatToFFIDefinition(format)
    local definition = ""

    for i, v in ipairs(format) do
        local name = v.name

        if name == "VertexTexCoord" then
            name = "VertexTexCoord_0"
        end

        definition = definition .. "\t" .. v.format .. " " .. name .. ";\n"
    end

    if not formats[definition] then
        local formatName = "vertexFormat" .. timesFFIVertexFormatCreated
        local def = "typedef struct {" ..
            definition .. "} " .. formatName .. ";"

        ffi.cdef(def)

        timesFFIVertexFormatCreated = timesFFIVertexFormatCreated + 1

        formats[definition] = { def, formatName }
    end

    return unpack(formats[definition])
end

formatToLocation = {
    VertexPosition    = 0,
    VertexNormal      = 1,
    VertexTangent     = 2,
    VertexJoints      = 3,
    VertexWeights     = 4,

    InstancedPosition = 5,
    InstancedRotation = 6,
    InstancedScale    = 7,
    InstancedColor    = 8,

    VertexTexCoord    = 9,
    VertexTexCoord_0  = 9,
    VertexTexCoord_1  = 10,
    VertexTexCoord_2  = 11,
    VertexTexCoord_3  = 12,
    VertexTexCoord_4  = 13,
    VertexTexCoord_5  = 14,
    VertexTexCoord_6  = 15,
    VertexTexCoord_7  = 16,
}

function fillFormatLocations(format)
    for i, v in pairs(format) do
        format[i].location = formatToLocation[v.name]
    end
end

local function printTableInternal(str, t, names, loopedTables)
    str = str .. ("  "):rep(#names) .. (names[#names] and names[#names] .. ": " or "") .. "{\n"
    for i, v in pairs(t) do
        if type(v) == "table" then
            if loopedTables[v] then
                str = str .. ("  "):rep(#names + 1) .. i .. " = Reference to " .. loopedTables[v] .. "\n"
                goto continue
            end
            if v == t or v == _G then
                str = str .. ("  "):rep(#names + 1) .. i .. " = Reference to self\n"
                goto continue
            end
            if not next(v) then
                str = str .. ("  "):rep(#names + 1) .. i .. " = {}\n"
                goto continue
            end
            table.insert(names, i)
            loopedTables[v] = table.concat(names, ".")
            printTableInternal(str, v, names, loopedTables)
            table.remove(names, #names)
        else
            str = str .. ("  "):rep(#names + 1) .. tostring(i) .. " = " .. tostring(v) .. "\n"
        end
        ::continue::
    end
    str = str .. ("  "):rep(#names) .. "}\n"

    return str
end

function printTable(t, skip)
    skip = skip or {}
    local names = {}
    if type(t) == "table" then
        local str = ""
        io.write(printTableInternal(str, t, names, skip))
    else
        io.write(tostring(t) .. "\n")
    end
end

Updated = true
DebugView = false
DebugMode = 0
Frame = 0

function love.update(dt)
    if love.mouse.isDown(2) then
        Updated = true

        Camera.previousInverseViewMatrix:set(Camera.inverseViewMatrix)
        Camera.previousInverseViewProjectionMatrix:set(Camera.inverseViewProjectionMatrix)
        Camera.previousProjectionMatrix:set(Camera.projectionMatrix)
        Camera.previousViewMatrix:set(Camera.viewMatrix)
        Camera.previousViewProjectionMatrix:set(Camera.viewProjectionMatrix)

        Rhodium.math.calculateCameraMatrix()
        Camera.inverseViewMatrix = Camera.viewMatrix:invert():transpose()
        Camera.inverseViewProjectionMatrix = Camera.viewProjectionMatrix:invert():transpose()
        local forward = vec3(math.sin(-Camera.yaw) * math.cos(Camera.pitch),
            math.sin(Camera.pitch), math.cos(-Camera.yaw) * math.cos(Camera.pitch))
        local right = vec3(math.cos(Camera.yaw), 0, math.sin(Camera.yaw))
        if love.keyboard.isDown("w") then
            Camera.position = Camera.position -
                forward * Camera.speed * dt
        end
        if love.keyboard.isDown("s") then
            Camera.position = Camera.position +
                forward * Camera.speed * dt
        end
        if love.keyboard.isDown("space") then
            Camera.position.y = Camera.position.y +
                Camera.speed * dt
        end
        if love.keyboard.isDown("lctrl") then
            Camera.position.y = Camera.position.y -
                Camera.speed * dt
        end
        if love.keyboard.isDown("d") then
            Camera.position = Camera.position +
                right * Camera.speed * dt
        end
        if love.keyboard.isDown("a") then
            Camera.position = Camera.position -
                right * Camera.speed * dt
        end
    end
end

function love.draw()
    if Updated then
        Updated = false
        Frame = 0

        love.graphics.setCanvas(Target)
        love.graphics.clear()

        love.graphics.setCanvas()
    else
        Frame = Frame + 1
    end

    local sx, sy, sz = Shaders.rayInit:getLocalThreadgroupSize()

    Shaders.rayInit:send("CameraPosition", Camera.position:ttable())
    Shaders.rayInit:send("InverseViewProjectionMatrix", "column", Camera.inverseViewProjectionMatrix)
    Shaders.rayInit:send("ScreenSize", { love.graphics.getDimensions() })
    Shaders.rayInit:send("RandomIndex", love.math.random(0, 2 ^ 32 - 1))

    local iW, iH = love.graphics.getDimensions()
    local x, y = math.ceil(iW / sx), math.ceil(iH / sy)

    Rhodium.graphics.dispatchThreadgroups(Shaders.rayInit, x, y, 1)

    local sx, sy, sz = Shaders.rayTrace:getLocalThreadgroupSize()

    Shaders.rayTrace:send("RandomIndex", love.math.random(0, 2 ^ 32 - 1))

    local iW, iH = love.graphics.getDimensions()
    local x = math.ceil((iW * iH) / sx)

    for i = 1, MAX_BOUNCES do
        Rhodium.graphics.dispatchThreadgroups(Shaders.rayTrace, x, 1, 1)
    end

    local sx, sy, sz = Shaders.rayWrite:getLocalThreadgroupSize()
    Shaders.rayWrite:send("FrameIndex", Frame)
    Shaders.rayWrite:send("ScreenSize", { love.graphics.getDimensions() })
    Shaders.rayWrite:send("CurrentFrame", Target)

    local iW, iH = love.graphics.getDimensions()
    local x, y = math.ceil(iW / sx), math.ceil(iH / sy)

    Rhodium.graphics.dispatchThreadgroups(Shaders.rayWrite, x, y, 1)

    love.graphics.draw(Target)

    love.graphics.print("FPS: " .. love.timer.getFPS(), 10, 10)
    love.graphics.print("Camera position: " .. tostring(Camera.position), 10, 30)
    love.graphics.print("Debug view: " .. tostring(DebugView), 10, 50)
    local modeString = "None"
    if DebugView then
        if DebugMode == 0 then
            modeString = "Bounding box intersection tests"
        elseif DebugMode == 1 then
            modeString = "Triangle intersection tests"
        elseif DebugMode == 2 then
            modeString = "None"
        end
    end

    love.graphics.print("Debug mode: " .. tostring(DebugMode) .. "(" .. modeString .. ")", 10, 70)
end

function love.mousemoved(x, y, dx, dy)
    if love.mouse.isDown(2) then
        Updated = true

        Camera.yaw = Camera.yaw + dx * 0.004
        Camera.pitch = Rhodium.math.clamp(Camera.pitch + dy * 0.004, -PI05, PI05)
    end
end

function love.keypressed(key)
    if key == "escape" then
        love.event.quit()
    end

    if key == "f1" then
        DebugView = not DebugView
    end

    if key == "f2" then
        DebugMode = (DebugMode + 1) % 3
    end
end
