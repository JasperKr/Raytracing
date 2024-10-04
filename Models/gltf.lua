--[[
-------------------------------------------------------------------------------
	Menori
	@author rozenmad
	2023
-------------------------------------------------------------------------------
]]

--[[--
Module for load the *gltf format.
Separated GLTF (.gltf+.bin+textures) or (.gltf+textures) is supported now.
]]
-- @module glTFLoader

local json = require("Models.json")

local function getFFIPointer(data)
	if data.getFFIPointer then return data:getFFIPointer() else return data:getPointer() end
end

local glTFLoader = {}

local type_constants = {
	['SCALAR'] = 1,
	['VEC2'] = 2,
	['VEC3'] = 3,
	['VEC4'] = 4,
	['MAT2'] = 4,
	['MAT3'] = 9,
	['MAT4'] = 16,
}

local ffi_indices_types = {
	[5121] = 'unsigned char*',
	[5123] = 'unsigned short*',
	[5125] = 'unsigned int*',
}

local component_type_constants = {
	[5120] = 1,
	[5121] = 1,
	[5122] = 2,
	[5123] = 2,
	[5125] = 4,
	[5126] = 4,
}

local component_types = {
	[5120] = 'int8',
	[5121] = 'uint8',
	[5122] = 'int16',
	[5123] = 'unorm16',
	[5125] = 'uint32',
	[5126] = 'float',
}

local add_vertex_format
---@diagnostic disable-next-line: undefined-field
if love._version_major > 11 then
	local types = {
		['SCALAR'] = '',
		['VEC2'] = 'vec2',
		['VEC3'] = 'vec3',
		['VEC4'] = 'vec4',
		['MAT2'] = 'mat2x2',
		['MAT3'] = 'mat3x3',
		['MAT4'] = 'mat4x4',
	}
	function add_vertex_format(vertexformat, attribute_name, buffer)
		local format = component_types[buffer.component_type] .. types[buffer.type]
		table.insert(vertexformat, {
			name = attribute_name, format = format
		})
	end
else
	local types = {
		'byte',
		'unorm16',
		'',
		'float',
	}

	function add_vertex_format(vertexformat, attribute_name, buffer)
		table.insert(vertexformat, {
			attribute_name, types[buffer.component_size], buffer.type_elements_count
		})
	end
end


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

---@param component_type integer
---@return 'b'|'B'|'h'|'H'|'I4'|'f'|nil
local function get_unpack_type(component_type)
	if
		component_type == 5120 then
		return 'b'
	elseif
		component_type == 5121 then
		return 'B'
	elseif
		component_type == 5122 then
		return 'h'
	elseif
		component_type == 5123 then
		return 'H'
	elseif
		component_type == 5125 then
		return 'I4'
	elseif
		component_type == 5126 then
		return 'f'
	end
end

---@param data table
---@param buffers table
---@param accessor_index integer
---@return table
local function get_buffer(data, buffers, accessor_index)
	local accessor = data.accessors[accessor_index + 1]
	local buffer_view = data.bufferViews[accessor.bufferView + 1]

	local bufferData = buffers[buffer_view.buffer + 1]

	local offset = buffer_view.byteOffset or 0
	local length = buffer_view.byteLength

	local component_size = component_type_constants[accessor.componentType]
	local type_elements_count = type_constants[accessor.type]
	return {
		data = bufferData,
		offset = offset + (accessor.byteOffset or 0),
		length = length,

		stride = buffer_view.byteStride or (component_size * type_elements_count),
		component_size = component_size,
		component_type = accessor.componentType,
		type = accessor.type,

		type_elements_count = type_elements_count,
		count = accessor.count,

		min = accessor.min,
		max = accessor.max,
	}
end

---@param data table
---@param buffers table
---@param v table
---@return table, integer, integer, integer
local function get_indices_content(data, buffers, v)
	local buffer = get_buffer(data, buffers, v)
	local element_size = buffer.component_size * buffer.type_elements_count

	local min = buffer.min and buffer.min[1] or 0
	local max = buffer.max and buffer.max[1] or 0

	local uint8 = element_size < 2

	local temp_data
	if ffi and not uint8 then
		temp_data = love.data.newByteData(buffer.count * element_size)
		local temp_data_pointer = ffi.cast('char*', getFFIPointer(temp_data))
		local data = ffi.cast('char*', getFFIPointer(buffer.data)) + buffer.offset

		for i = 0, buffer.count - 1 do
			ffi.copy(temp_data_pointer + i * element_size, data + i * buffer.stride, element_size)
			local value = ffi.cast(ffi_indices_types[buffer.component_type], temp_data_pointer + i * element_size)[0]
			if value > max then max = value end
			if value < min then min = value end
		end

		for i = 0, buffer.count - 1 do
			local ptr = ffi.cast(ffi_indices_types[buffer.component_type], temp_data_pointer + i * element_size)
			ptr[0] = ptr[0] - min
		end
	else
		temp_data = {}
		local data_string = buffer.data:getString()
		local unpack_type = get_unpack_type(buffer.component_type)

		for i = 0, buffer.count - 1 do
			local pos = buffer.offset + i * element_size + 1
			local value = love.data.unpack(unpack_type, data_string, pos)

			---@cast value number

			temp_data[i + 1] = value + 1
			if value > max then max = value end
			if value < min then min = value end
		end

		for i = 0, buffer.count - 1 do
			temp_data[i + 1] = temp_data[i + 1] - min
		end
	end
	return temp_data, element_size, min, max
end

---@param attribute_buffers table
---@param components_stride integer
---@param length integer
---@param min integer
---@param max integer
---@return table
local function get_vertices_content(attribute_buffers, components_stride, length, min, max)
	local start_offset = 0
	local temp_data

	if ffi then
		temp_data = love.data.newByteData(length)
		local temp_data_pointer = ffi.cast('char*', getFFIPointer(temp_data))

		for _, buffer in ipairs(attribute_buffers) do
			local element_size = buffer.component_size * buffer.type_elements_count
			for i = min, max do
				local p1 = buffer.offset + i * buffer.stride
				local data = ffi.cast('char*', getFFIPointer(buffer.data)) + p1

				local p2 = start_offset + (i - min) * components_stride
				ffi.copy(temp_data_pointer + p2, data, element_size)
			end
			start_offset = start_offset + element_size
		end
	else
		temp_data = {}
		for _, buffer in ipairs(attribute_buffers) do
			local unpack_type = get_unpack_type(buffer.component_type)
			local data_string = buffer.data:getString()
			for i = min, max do
				local vertex = temp_data[(i - min) + 1] or {}
				temp_data[(i - min) + 1] = vertex

				local pos = buffer.offset + i * buffer.stride
				for k = 0, buffer.type_elements_count - 1 do
					local element_pos = pos + k * buffer.component_size + 1
					local attr = love.data.unpack(unpack_type, data_string, element_pos)
					vertex[start_offset + k + 1] = attr
				end
			end
			start_offset = start_offset + buffer.type_elements_count
		end
	end

	return temp_data
end

---@param data table
---@param buffers table
---@param mesh table
---@return table
local function init_mesh(data, buffers, mesh)
	local primitives = {}
	for j, primitive in ipairs(mesh.primitives) do
		local indices, indicesTSize, min, max
		if primitive.indices then
			indices, indicesTSize, min, max = get_indices_content(data, buffers, primitive.indices)
		else
			min = 0
			max = 0
		end

		local length = 0
		local components_stride = 0
		local attribute_buffers = {}
		local count = 0
		local vertexformat = {}

		for k, v in pairs(primitive.attributes) do
			local attribute, value = k:match('(%w+)(.*)')
			local name
			if attribute_aliases[attribute] then
				name = attribute_aliases[attribute] .. value
			else
				name = k
			end

			local buffer = get_buffer(data, buffers, v)
			attribute_buffers[#attribute_buffers + 1] = buffer

			if max == 0 then max = buffer.count end
			if ffi then
				count = (max - min) + 1
			else
				count = (max - min)
			end

			local element_size = buffer.component_size * buffer.type_elements_count
			length = length + count * element_size
			components_stride = components_stride + element_size

			add_vertex_format(vertexformat, name, buffer)
		end

		local vertices = get_vertices_content(attribute_buffers, components_stride, length, min, max)

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
local function texture(textures, t)
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
		main_texture = texture(textures, pbr.baseColorTexture)
		local metallicRoughnessTexture = texture(textures, pbr.metallicRoughnessTexture)

		if main_texture then
			if love.graphics then
				main_texture.source = love.graphics.newTexture(main_texture.data, {
					debugname = main_texture.source:getDebugName(), linear = false, mipmaps = false, format = "srgba8"
				})
			else
				main_texture.creationData = { main_texture.data, { debugname = main_texture.creationData[2].debugname, linear = false, mipmaps = false, format = "srgba8" } }
			end

			uniforms.mainTexCoord = main_texture.tcoord
		end
		if metallicRoughnessTexture then
			uniforms.metallicRoughnessTexture = metallicRoughnessTexture.source
			uniforms.metallicRoughnessTextureData = metallicRoughnessTexture.data
			uniforms.metallicRoughnessTextureCoord = metallicRoughnessTexture.tcoord
		end

		uniforms.metalness = pbr.metallicFactor or 0.0
		uniforms.roughness = pbr.roughnessFactor or 1.0
	end

	if material.normalTexture then
		local normalTexture = texture(textures, material.normalTexture)
		if normalTexture then
			uniforms.normalTexture = normalTexture.source
			uniforms.normalTextureData = normalTexture.data
			uniforms.normalTextureCoord = normalTexture.tcoord
			uniforms.normalTextureScale = normalTexture.scale
		end
	end

	if material.occlusionTexture then
		local occlusionTexture = texture(textures, material.occlusionTexture)
		if occlusionTexture then
			uniforms.occlusionTexture = occlusionTexture.source
			uniforms.occlusionTextureData = occlusionTexture.data
			uniforms.occlusionTextureCoord = occlusionTexture.tcoord
			uniforms.occlusionTextureStrength = occlusionTexture.strength
		end
	end

	if material.emissiveTexture then
		local emissiveTexture = texture(textures, material.emissiveTexture)

		if emissiveTexture then
			if love.graphics then
				emissiveTexture.source = love.graphics.newTexture(emissiveTexture.data, {
					debugname = emissiveTexture.source:getDebugName(), linear = false, mipmaps = false, format = "srgba8"
				})
			else
				emissiveTexture.creationData = { emissiveTexture.data, { debugname = emissiveTexture.source:getDebugName(), linear = false, mipmaps = false, format = "srgba8" } }
			end

			uniforms.emissiveTexture = emissiveTexture.source
			uniforms.emissiveTextureData = emissiveTexture.data
			uniforms.emissiveTextureCoord = emissiveTexture.tcoord
		end
	end
	uniforms.emissiveFactor = material.emissiveFactor or { 0, 0, 0 }

	uniforms.emissiveFactor[1] = uniforms.emissiveFactor[1] * 100.0 -- make emissive factor brighter
	uniforms.emissiveFactor[2] = uniforms.emissiveFactor[2] * 100.0
	uniforms.emissiveFactor[3] = uniforms.emissiveFactor[3] * 100.0

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
local function parse_filter(value)
	if
		value == 9728 then
		return 'nearest'
	elseif
		value == 9729 then
		return 'linear'
	else
		return 'linear'
	end
end

---@param value integer
---@return 'clamp'|'mirroredrepeat'|'repeat'
local function parse_wrap(value)
	if value == 33071 then
		return 'clamp'
	elseif value == 33648 then
		return 'mirroredrepeat'
	elseif value == 10497 then
		return 'repeat'
	else
		return 'repeat'
	end
end

---@param buffer table
---@return table
local function get_data_array(buffer)
	local array = {}
	if ffi then
		for i = 0, buffer.count - 1 do
			local data_offset = ffi.cast('char*', getFFIPointer(buffer.data)) + buffer.offset + i * buffer.stride
			local ptr = ffi.cast('float*', data_offset)
			if buffer.type_elements_count > 1 then
				local vector = {}
				for j = 1, buffer.type_elements_count do
					local value = ptr[j - 1]
					table.insert(vector, value)
				end
				table.insert(array, vector)
			else
				table.insert(array, ptr[0])
			end
		end
	end
	return array
end

---@param data table
---@param buffers table
---@param animation table
---@return table
local function read_animation(data, buffers, animation)
	local samplers = {}
	for i, v in ipairs(animation.samplers) do
		local time_buffer = get_buffer(data, buffers, v.input)
		local data_buffer = get_buffer(data, buffers, v.output)

		table.insert(samplers, {
			time_array = get_data_array(time_buffer),
			data_array = get_data_array(data_buffer),
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
---@param io_read function
---@param path string
---@param images table
---@param texture table
---@return table
local function load_image(data, buffers, io_read, path, images, texture)
	local source = texture.source + 1
	local MSFT_texture_dds = texture.extensions and texture.extensions.MSFT_texture_dds
	if MSFT_texture_dds then
		source = MSFT_texture_dds.source + 1
	end

	-- hack to support manually compressed images using compressonator
	local image = images[source]

	if image.uri then
		local image_directory = image.uri:match("(.*/)") or ""

		local hasDDSOutputDir = love.filesystem.exists(path .. image_directory .. "results")
		if hasDDSOutputDir then
			local path_to_check = image_directory ..
				"results/" .. (image.uri:match(".*/(.*)%.%w+$") or image.uri:match("(.*)%.%w+$")) .. ".DDS"
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
			image_raw_data = love.filesystem.newFileData(io_read(image_filename), image_filename)
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

local function unpack_data(format, data, iterator)
	local pos = iterator.position
	iterator.position = iterator.position + love.data.getPackedSize(format)
	return love.data.unpack(format, data, pos + 1)
end

local function glb_parser(glb_data)
	local iterator = {
		position = 0
	}
	local magic, version = unpack_data('<I4I4', glb_data, iterator)
	assert(magic == 0x46546C67, 'GLB: wrong magic!')
	assert(version == 0x2, 'Supported only GLTF 2.0!')

	local length = unpack_data('<I4', glb_data, iterator)

	local json_data
	local buffers = {}
	local bufferi = 1

	while iterator.position < length do
		local chunk_length, chunk_type = unpack_data('<I4I4', glb_data, iterator)
		local start_position = iterator.position
		if
			chunk_type == 0x4E4F534A then
			local data_view = love.data.newDataView(glb_data, iterator.position, chunk_length)
			json_data = json.decode(data_view:getString())
		elseif
			chunk_type == 0x004E4942 then
			local data_view = love.data.newDataView(glb_data, iterator.position, chunk_length)
			buffers[bufferi] = data_view
			bufferi = bufferi + 1
		end

		iterator.position = start_position + chunk_length
	end

	return json_data, buffers
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
--- @param filename string The filepath to the gltf file (GLTF must be separated (.gltf+.bin+textures) or (.gltf+textures))
--- @return table
function glTFLoader.load(filename)
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
		data, buffers = glb_parser(filedata)
	end

	local samplers = {}
	if data.samplers then
		for _, v in ipairs(data.samplers) do
			table.insert(samplers, {
				magFilter = parse_filter(v.magFilter),
				minFilter = parse_filter(v.minFilter),
				wrapS = parse_wrap(v.wrapS),
				wrapT = parse_wrap(v.wrapT),
			})
		end
	end

	local images = {}
	local textures = {}
	if data.textures then
		for _, texture in ipairs(data.textures) do
			local sampler
			if texture.sampler then
				sampler = samplers[texture.sampler + 1]
			end
			local image = images[texture.source + 1]

			if not image then
				image = load_image(data, buffers, io_read, path, data.images, texture)
				images[texture.source + 1] = image
			end

			table.insert(textures, {
				image = image, sampler = sampler
			})

			if sampler and love.graphics then
				image.source:setFilter(sampler.magFilter, sampler.minFilter)
				image.source:setWrap(sampler.wrapS, sampler.wrapT)
			end
		end
	end

	local skins = {}
	if data.skins then
		for _, v in ipairs(data.skins) do
			local buffer = get_buffer(data, buffers, v.inverseBindMatrices)
			table.insert(skins, {
				inverse_bind_matrices = get_data_array(buffer),
				joints = v.joints,
				skeleton = v.skeleton,
			})
		end
	end

	local materials = {}
	if data.materials then
		for i, v in ipairs(data.materials) do
			materials[i] = createMaterial(textures, v)
		end
	end

	local meshes = {}
	for i, v in ipairs(data.meshes) do
		meshes[i] = init_mesh(data, buffers, v)
	end

	local animations = {}
	if data.animations then
		for i, animation in ipairs(data.animations) do
			animations[i] = {
				channels = read_animation(data, buffers, animation),
				name = animation.name
			}
		end
	end

	local lights = {}
	if data.extensions and data.extensions.KHR_lights_punctual then
		for i, light in ipairs(data.extensions.KHR_lights_punctual.lights) do
			lights[i] = load_light(light)
		end
	end

	return {
		asset = data.asset,
		nodes = data.nodes,
		scene = data.scene,
		materials = materials,
		meshes = meshes,
		scenes = data.scenes,
		images = images,
		animations = animations,
		skins = skins,
		lights = lights,

		data = data,
		buffers = buffers,
	}
end

return glTFLoader
