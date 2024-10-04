--[[
-------------------------------------------------------------------------------
	Menori
	@author rozenmad
	2023
-------------------------------------------------------------------------------
]]

-- reworked by Jasper Krocké 2024 for multithreaded loading

--[[--
Module for load the *gltf format.
Separated GLTF (.gltf+.bin+textures) or (.gltf+textures) is supported now.
]]

if not table.new then
    table.new = require("table.new")
end

---@class gltf.buffer
---@field data love.ByteData
---@field offset integer
---@field length integer
---@
---@field stride integer
---@field componentSize integer
---@field componentType integer
---@field type string
---@
---@field typeComponentCount integer
---@field count integer
---@
---@field min {[1]:integer}
---@field max {[1]:integer}

local json = require("Models.json")

local typeToComponentCounts = {
    ['SCALAR'] = 1,
    ['VEC2'] = 2,
    ['VEC3'] = 3,
    ['VEC4'] = 4,
    ['MAT2'] = 4,
    ['MAT3'] = 9,
    ['MAT4'] = 16,
}

local ffiIndicesTypes = {
    [5121] = 'unsigned char*',
    [5123] = 'unsigned short*',
    [5125] = 'unsigned int*',
}

local typeToSize = {
    [5120] = 1,
    [5121] = 1,
    [5122] = 2,
    [5123] = 2,
    [5125] = 4,
    [5126] = 4,
}

local componentTypes = {
    [5120] = 'int8',
    [5121] = 'uint8',
    [5122] = 'int16',
    [5123] = 'unorm16', -- uint16
    [5125] = 'uint32',
    [5126] = 'float',
}

local types = {
    ['SCALAR'] = '',
    ['VEC2'] = 'vec2',
    ['VEC3'] = 'vec3',
    ['VEC4'] = 'vec4',
    ['MAT2'] = 'mat2x2',
    ['MAT3'] = 'mat3x3',
    ['MAT4'] = 'mat4x4',
}


local attribute_aliases = {
    ['POSITION'] = 'VertexPosition',
    ['TEXCOORD'] = 'VertexTexCoord',
    ['JOINTS']   = 'VertexJoints',
    ['NORMAL']   = 'VertexNormal',
    ['COLOR']    = 'VertexColor',
    ['WEIGHTS']  = 'VertexWeights',
    ['TANGENT']  = 'VertexTangent',
}

---@param mode integer
---@return 'vertices'|'strip'|'fan'|'triangles'
local function get_primitive_modes_constants(mode)
    if mode == 0 then
        return 'vertices'
    elseif mode == 1 then
    elseif mode == 2 then
    elseif mode == 3 then
    elseif mode == 5 then
        return 'strip'
    elseif mode == 6 then
        return 'fan'
    end
    return 'triangles'
end

---@param componentType integer
---@return 'b'|'B'|'h'|'H'|'I4'|'f'|nil
local function get_unpack_type(componentType)
    if
        componentType == 5120 then
        return 'b'
    elseif
        componentType == 5121 then
        return 'B'
    elseif
        componentType == 5122 then
        return 'h'
    elseif
        componentType == 5123 then
        return 'H'
    elseif
        componentType == 5125 then
        return 'I4'
    elseif
        componentType == 5126 then
        return 'f'
    end
end

---@param data table
---@param buffers table
---@param accessorIndex integer
---@return gltf.buffer
local function getBuffer(data, buffers, accessorIndex)
    local accessor = data.accessors[accessorIndex + 1]
    local bufferView = data.bufferViews[accessor.bufferView + 1]

    local bufferData = buffers[bufferView.buffer + 1]

    local offset = bufferView.byteOffset or 0
    local length = bufferView.byteLength

    local componentSize = typeToSize[accessor.componentType]
    local typeComponentCount = typeToComponentCounts[accessor.type]
    return {
        data = bufferData,
        offset = offset + (accessor.byteOffset or 0),
        length = length,

        stride = bufferView.byteStride or (componentSize * typeComponentCount),
        componentSize = componentSize,
        componentType = accessor.componentType,
        type = accessor.type,

        typeComponentCount = typeComponentCount,
        count = accessor.count,

        min = accessor.min,
        max = accessor.max,
    }
end

---@param data table
---@param buffers table
---@param v table
---@return table, integer, integer, integer
local function getIndices(data, buffers, v)
    local buffer = getBuffer(data, buffers, v)
    local elementSize = buffer.componentSize * buffer.typeComponentCount

    local min = buffer.min and buffer.min[1] or 0
    local max = buffer.max and buffer.max[1] or 0

    local uint8 = elementSize < 2

    local tempData
    if not uint8 then
        tempData = love.data.newByteData(buffer.count * elementSize)
        local tempPtr = ffi.cast('char*', tempData:getFFIPointer())
        local dataPtr = ffi.cast('char*', buffer.data:getFFIPointer()) + buffer.offset

        for i = 0, buffer.count - 1 do
            ffi.copy(tempPtr + i * elementSize, dataPtr + i * buffer.stride, elementSize)
            local value = ffi.cast(ffiIndicesTypes[buffer.componentType], tempPtr + i * elementSize)[0]
            if value > max then max = value end
            if value < min then min = value end
        end

        for i = 0, buffer.count - 1 do
            local ptr = ffi.cast(ffiIndicesTypes[buffer.componentType], tempPtr + i * elementSize)
            ptr[0] = ptr[0] - min
        end
    else
        tempData = table.new(data.count, 0)
        local data_string = buffer.data:getString()
        local unpack_type = get_unpack_type(buffer.componentType)

        for i = 0, buffer.count - 1 do
            local pos = buffer.offset + i * elementSize + 1
            local value = love.data.unpack(unpack_type, data_string, pos)

            ---@cast value number

            tempData[i + 1] = value + 1
            if value > max then max = value end
            if value < min then min = value end
        end

        for i = 0, buffer.count - 1 do
            tempData[i + 1] = tempData[i + 1] - min
        end
    end
    return tempData, elementSize, min, max
end

---@param buffers {[1]:gltf.buffer}
---@param stride integer
---@param length integer
---@param min integer
---@param max integer
---@return love.ByteData
local function getVertices(buffers, stride, length, min, max)
    local offset = 0

    local byteData = love.data.newByteData(length)
    local ptr = ffi.cast('char*', byteData:getFFIPointer())

    for _, buffer in ipairs(buffers) do
        local elementSize = buffer.componentSize * buffer.typeComponentCount
        for i = min, max do
            local p1 = buffer.offset + i * buffer.stride
            local data = ffi.cast('char*', buffer.data:getFFIPointer()) + p1

            local p2 = offset + (i - min) * stride
            ffi.copy(ptr + p2, data, elementSize)
        end
        offset = offset + elementSize
    end


    return byteData
end

---@param data table
---@param buffers table
---@param mesh table
---@return table
local function initMesh(data, buffers, mesh)
    local primitives = {}
    for j, primitive in ipairs(mesh.primitives) do
        local indices, indicesTSize, min, max
        if primitive.indices then
            indices, indicesTSize, min, max = getIndices(data, buffers, primitive.indices)
        else
            min = 0
            max = 0
        end

        local length = 0
        local stride = 0
        local attributeBuffers = {}

        local count = 0
        local vertexformat = {}

        for attributeData, index in pairs(primitive.attributes) do
            local attribute, value = attributeData:match('(%w+)(.*)')
            local name
            if attribute_aliases[attribute] then
                name = attribute_aliases[attribute] .. value
            else
                name = attributeData
            end

            local buffer = getBuffer(data, buffers, index)
            attributeBuffers[#attributeBuffers + 1] = buffer

            if max == 0 then max = buffer.count end

            count = (max - min) + 1

            local elementSize = buffer.componentSize * buffer.typeComponentCount
            length = length + count * elementSize
            stride = stride + elementSize

            table.insert(vertexformat, {
                name = name, format = componentTypes[buffer.componentType] .. types[buffer.type]
            })
        end

        local vertices = getVertices(attributeBuffers, stride, length, min, max)

        primitives[j] = {
            mode = get_primitive_modes_constants(primitive.mode),
            vertexformat = vertexformat,
            vertices = vertices,
            indices = indices,
            indicesTSize = indicesTSize,
            materialIndex = primitive.material,
            count = count,
        }
    end
    return {
        primitives = primitives,
        name = mesh.name,
    }
end

---@param textures table
---@param t table
---@return table?
local function createTexture(textures, t)
    if t then
        local tex = textures[t.index + 1]

        assert(tex, "Cannot find texture with index: " .. t.index + 1)

        local ret = {
            texture      = tex,
            source       = tex.image.source,
            data         = tex.image.data,
            creationData = tex.image.creationData,
        }
        for k, v in pairs(t) do
            if k ~= 'index' then
                ret[k] = v
            end
        end
        return ret
    end
end

---@param textures table
---@param material table
---@return table
local function createMaterial(textures, material)
    local uniforms = {}

    local main_texture
    local pbr = material.pbrMetallicRoughness
    uniforms.baseColor = (pbr and pbr.baseColorFactor) or { 1, 1, 1, 1 }
    if pbr then
        main_texture = createTexture(textures, pbr.baseColorTexture)
        local metallicRoughnessTexture = createTexture(textures, pbr.metallicRoughnessTexture)

        if main_texture then
            if main_texture.source and main_texture.source.release then
                main_texture.source:release()
            end

            main_texture.source = love.graphics.newTexture(main_texture.data,
                {
                    debugname = main_texture.creationData[2].debugname,
                    linear = false,
                    mipmaps = false,
                    format = "srgba8"
                }
            )

            uniforms.mainTexCoord = main_texture.tcoord
        end
        if metallicRoughnessTexture then
            uniforms.metallicRoughnessTexture = metallicRoughnessTexture.source
            uniforms.metallicRoughnessTextureData = metallicRoughnessTexture.data
            uniforms.metallicRoughnessTextureCoord = metallicRoughnessTexture.tcoord
        end

        uniforms.metalness = pbr.metallicFactor or 1.0
        uniforms.roughness = pbr.roughnessFactor or 1.0
    end

    if material.normalTexture then
        local normalTexture = createTexture(textures, material.normalTexture)
        if normalTexture then
            uniforms.normalTexture = normalTexture.source
            uniforms.normalTextureData = normalTexture.data
            uniforms.normalTextureCoord = normalTexture.tcoord
            uniforms.normalTextureScale = normalTexture.scale
        end
    end

    if material.occlusionTexture then
        local occlusionTexture = createTexture(textures, material.occlusionTexture)
        if occlusionTexture then
            uniforms.occlusionTexture = occlusionTexture.source
            uniforms.occlusionTextureData = occlusionTexture.data
            uniforms.occlusionTextureCoord = occlusionTexture.tcoord
            uniforms.occlusionTextureStrength = occlusionTexture.strength
        end
    end

    if material.emissiveTexture then
        local emissiveTexture = createTexture(textures, material.emissiveTexture)
        if emissiveTexture then
            if emissiveTexture.source and emissiveTexture.source.release then
                emissiveTexture.source:release()
            end

            emissiveTexture.source = love.graphics.newTexture(emissiveTexture.data,
                {
                    debugname = emissiveTexture.creationData[2].debugname,
                    linear = false,
                    mipmaps = false,
                    format = "srgba8"
                }
            )

            uniforms.emissiveTexture = emissiveTexture.source
            uniforms.emissiveTextureData = emissiveTexture.data
            uniforms.emissiveTextureCoord = emissiveTexture.tcoord
        end
    end
    uniforms.emissiveFactor = material.emissiveFactor or { 0, 0, 0 }
    uniforms.opaque = material.alphaMode == 'OPAQUE' or not material.alphaMode
    if material.alphaMode == 'MASK' then
        uniforms.alphaCutoff = material.alphaCutoff or 0.5
    else
        uniforms.alphaCutoff = 0.0
    end

    return {
        name = material.name,
        main_texture = main_texture,
        uniforms = uniforms,
        double_sided = material.doubleSided,
        alpha_mode = material.alphaMode or 'OPAQUE',
    }
end

---@param value integer
---@return 'nearest'|'linear'
local function parseFilter(value)
    if value == 9728 then
        return 'nearest'
    elseif value == 9729 then
        return 'linear'
    else
        return 'linear'
    end
end

---@param value integer
---@return 'clamp'|'mirroredrepeat'|'repeat'
local function parseWrap(value)
    if value == 33071 then
        return 'clamp'
    elseif value == 33648 then
        return 'mirroredrepeat'
    else
        return 'repeat'
    end
end

---@param buffer gltf.buffer
---@return table
local function getDataArray(buffer)
    local array = {}

    for i = 0, buffer.count - 1 do
        local offset = ffi.cast('char*', buffer.data:getFFIPointer()) + buffer.offset + i * buffer.stride
        local ptr = ffi.cast('float*', offset)
        if buffer.typeComponentCount > 1 then
            local vector = {}
            for j = 1, buffer.typeComponentCount do
                local value = ptr[j - 1]
                table.insert(vector, value)
            end
            table.insert(array, vector)
        else
            table.insert(array, ptr[0])
        end
    end

    return array
end

---@param data table
---@param buffers table
---@param animation table
---@return table
local function readAnimation(data, buffers, animation)
    local samplers = {}
    for i, v in ipairs(animation.samplers) do
        local time_buffer = getBuffer(data, buffers, v.input)
        local data_buffer = getBuffer(data, buffers, v.output)

        table.insert(samplers, {
            time_array = getDataArray(time_buffer),
            data_array = getDataArray(data_buffer),
            interpolation = v.interpolation,
        })
    end

    local channels = {}
    for i, v in ipairs(animation.channels) do
        table.insert(channels, {
            sampler = samplers[v.sampler + 1],
            target_node = v.target.node,
            target_path = v.target.path,
        })
    end

    return channels
end

---@param data table
---@param buffers table
---@param path string
---@param images table
---@param texture table
---@return table
local function loadImage(data, buffers, path, images, texture)
    local source = texture.source + 1
    local MSFT_texture_dds = texture.extensions and texture.extensions.MSFT_texture_dds
    if MSFT_texture_dds then
        source = MSFT_texture_dds.source + 1
    end

    -- hack to support manually compressed images using compressonator
    local image = images[source]

    if image.uri then
        local image_directory = image.uri:match("(.*/)")

        local hasDDSOutputDir = love.filesystem.exists(path .. image_directory .. "results")
        if hasDDSOutputDir then
            local path_to_check = image_directory ..
                "results/" .. image.uri:match(".*/(.*)%.%w+$") .. ".DDS"
            local hasCompressedVersion = love.filesystem.exists(path .. path_to_check)

            if hasCompressedVersion then
                image.uri = path_to_check

                MSFT_texture_dds = true
            end
        end
    end

    local image_raw_data
    if image.uri then
        local base64data = image.uri:match('^data:image/.*;base64,(.+)')
        if base64data then
            image_raw_data = love.data.decode('data', 'base64', base64data)
        else
            local image_filename = path .. image.uri
            image_raw_data = love.filesystem.newFileData(image_filename, image_filename)
        end
    else
        local buffer_view = data.bufferViews[image.bufferView + 1]

        local data = buffers[buffer_view.buffer + 1]

        local offset = buffer_view.byteOffset or 0
        local length = buffer_view.byteLength

        image_raw_data = love.data.newDataView(data, offset, length)
    end

    local image_data
    if not MSFT_texture_dds then
        image_data = love.image.newImageData(image_raw_data)
    else
        image_data = love.image.newCompressedData(image_raw_data)
    end

    local image_source
    if love.graphics then
        image_source = love.graphics.newTexture(image_data, {
            debugname = image.name, linear = true, mipmaps = false
        })
    end

    return {
        source = image_source,
        creationData = { image_data, { debugname = image.name, linear = true, mipmaps = false } },
        data = image_data,
    }
end

local function unpackData(format, data, iterator)
    local pos = iterator.position
    iterator.position = iterator.position + love.data.getPackedSize(format)
    return love.data.unpack(format, data, pos + 1)
end

local function parseGlb(glb_data)
    local iterator = {
        position = 0
    }

    local magic, version = unpackData('<I4I4', glb_data, iterator)

    assert(magic == 0x46546C67, 'GLB: wrong magic!')
    assert(version == 0x2, 'Supported only GLTF 2.0!')

    local length = unpackData('<I4', glb_data, iterator)

    local data
    local buffers = {}
    local bufferi = 1

    while iterator.position < length do
        local chunk_length, chunk_type = unpackData('<I4I4', glb_data, iterator)
        local start_position = iterator.position
        if chunk_type == 0x4E4F534A then
            local dataView = love.data.newDataView(glb_data, iterator.position, chunk_length)
            data = json.decode(dataView:getString())
        elseif chunk_type == 0x004E4942 then
            local dataView = love.data.newDataView(glb_data, iterator.position, chunk_length)
            buffers[bufferi] = dataView
            bufferi = bufferi + 1
        end

        iterator.position = start_position + chunk_length
    end

    return data, buffers
end

---@param lightData table
---@return table lightData
local function load_light(lightData)
    local self = {
        name = lightData.name or "Light",
        color = lightData.color or { 1, 1, 1 },
        intensity = lightData.intensity or 1,
        type = lightData.type,
        range = lightData.range,
        spot = lightData.spot,
        direction = { 0, 0, -1 },
    }

    return self
end

--- Load gltf model by filename.
--- @param filename string The filepath to the gltf file (GLTF must be separated (.gltf+.bin+textures) or (.gltf+textures)
local function readFile(filename)
    local path = filename:match(".+/")
    local name, extension = filename:match("([^/]+)%.(.+)$")
    local io_read = love.filesystem.read
    assert(love.filesystem.getInfo(filename), 'in function <glTFLoader.load> file "' .. filename .. '" not found.')

    local data, buffers

    if extension == 'gltf' then
        local filedata = io_read(filename)
        data = json.decode(filedata)
        buffers = {}
        for i, v in ipairs(data.buffers) do
            local base64data = v.uri:match('^data:application/.*;base64,(.+)')
            if base64data then
                buffers[i] = love.data.decode('data', 'base64', base64data)
            else
                buffers[i] = love.data.newByteData(io_read(path .. v.uri))
            end
        end
    elseif extension == 'glb' then
        local filedata = io_read('data', filename)
        data, buffers = parseGlb(filedata)
    end

    return { data, buffers }
end

local function parseTexture(path, data, buffers, sampler, texture, index)
    local image = loadImage(data, buffers, path, data.images, texture)

    return {
        image = image,
        sampler = sampler,
        index = index,
        texture = texture
    }
end

local function parseTextures(mountPath, path, data, buffers, thread)
    local images = {}
    local textures = {}
    local samplers = {}

    local function parseTextureCallback(result)
        local image = result.image
        local sampler = result.sampler
        local texture = result.texture

        if image.creationData then
            image.source = love.graphics.newTexture(unpack(image.creationData))

            if sampler then
                image.source:setFilter(sampler.magFilter, sampler.minFilter)
                image.source:setWrap(sampler.wrapS, sampler.wrapT)
            end
        else
            error("Image creation data is missing")
        end

        images[texture.source + 1] = image
        textures[result.index] = {
            image = image, sampler = sampler
        }
    end

    if data.samplers then
        for _, v in ipairs(data.samplers) do
            table.insert(samplers, {
                magFilter = parseFilter(v.magFilter),
                minFilter = parseFilter(v.minFilter),
                wrapS = parseWrap(v.wrapS),
                wrapT = parseWrap(v.wrapT),
            })
        end
    end

    local imagesCreating = {}
    local index = 0
    local skippedWhileLoading = {}

    if data.textures then
        for _, texture in ipairs(data.textures) do
            local sampler
            if texture.sampler then
                sampler = samplers[texture.sampler + 1]
            end

            index = index + 1

            if not imagesCreating[texture.source + 1] then
                Rhodium.task.newTask(
                    thread, { mountPath, { path, data, buffers, sampler, texture, index }, "parseTexture" },
                    parseTextureCallback)

                imagesCreating[texture.source + 1] = true
            else
                table.insert(skippedWhileLoading, {
                    source = texture.source,
                    sampler = sampler,
                    index = index,
                })
            end
        end
    end

    return images, skippedWhileLoading, textures
end

local function parseTextures2(mountPath, path, data, buffers, thread)
    local images = {}
    local textures = {}
    local samplers = {}

    local function parseTextureCallback(result)
        local image = result.image
        local sampler = result.sampler
        local texture = result.texture

        if image.creationData then
            image.source = love.graphics.newTexture(unpack(image.creationData))

            if sampler then
                image.source:setFilter(sampler.magFilter, sampler.minFilter)
                image.source:setWrap(sampler.wrapS, sampler.wrapT)
            end
        else
            error("Image creation data is missing")
        end

        images[texture.source + 1] = image
        textures[result.index] = {
            image = image, sampler = sampler
        }
    end

    if data.samplers then
        for _, v in ipairs(data.samplers) do
            table.insert(samplers, {
                magFilter = parseFilter(v.magFilter),
                minFilter = parseFilter(v.minFilter),
                wrapS = parseWrap(v.wrapS),
                wrapT = parseWrap(v.wrapT),
            })
        end
    end

    local imagesCreating = {}
    local index = 0
    local skippedWhileLoading = {}

    if data.textures then
        for _, texture in ipairs(data.textures) do
            local sampler
            if texture.sampler then
                sampler = samplers[texture.sampler + 1]
            end

            index = index + 1

            if not imagesCreating[texture.source + 1] then
                parseTextureCallback(parseTexture(path, data, buffers, sampler, texture, index))

                imagesCreating[texture.source + 1] = true
            else
                table.insert(skippedWhileLoading, {
                    source = texture.source,
                    sampler = sampler,
                    index = index,
                })
            end
        end
    end

    return images, skippedWhileLoading, textures
end

local function parseSkins(data, buffers)
    local skins = {}
    if data.skins then
        for _, v in ipairs(data.skins) do
            local buffer = getBuffer(data, buffers, v.inverseBindMatrices)
            table.insert(skins, {
                inverse_bind_matrices = getDataArray(buffer),
                joints = v.joints,
                skeleton = v.skeleton,
            })
        end
    end

    return skins
end

local function parseMeshes(data, buffers)
    local meshes = {}
    for i, v in ipairs(data.meshes) do
        meshes[i] = initMesh(data, buffers, v)
    end

    return meshes
end

local function parseAnimations(data, buffers)
    local animations = {}
    if data.animations then
        for i, animation in ipairs(data.animations) do
            animations[i] = {
                channels = readAnimation(data, buffers, animation),
                name = animation.name
            }
        end
    end

    return animations
end

local function parseLights(data)
    local lights = {}
    if data.extensions and data.extensions.KHR_lights_punctual then
        for i, light in ipairs(data.extensions.KHR_lights_punctual.lights) do
            lights[i] = load_light(light)
        end
    end

    return lights
end

return {
    readFile = readFile,
    parseTextures = parseTextures,
    parseTextures2 = parseTextures2,
    parseSkins = parseSkins,
    parseMeshes = parseMeshes,
    parseAnimations = parseAnimations,
    parseLights = parseLights,
    parseTexture = parseTexture,
    createMaterial = createMaterial,
}
