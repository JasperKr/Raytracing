local function getDataType(size)
    if size < 2 then
        return 'uint8'
    elseif size == 2 then
        return 'uint16'
    else
        return 'uint32'
    end
end

local function getNodeTransform(node)
    local nodeMatrix = mat4()

    -- if the node has a matrix, use it
    if node.matrix then
        -- the matrix is stored as a table, so we need to unpack it
        nodeMatrix = mat4(unpack(node.matrix))

        -- translation
        local translation = vec3(nodeMatrix[4][1], nodeMatrix[4][2], nodeMatrix[4][3])

        -- rotation, but i'm not sure if this is correct
        -- since the matrix is for a different coordinate system
        local rotation = Rhodium.math.matrixToQuaternion(nodeMatrix):normalize()

        -- scale
        local scale = vec3(
            Rhodium.math.length3(nodeMatrix[1][1], nodeMatrix[1][2], nodeMatrix[1][3]),
            Rhodium.math.length3(nodeMatrix[2][1], nodeMatrix[2][2], nodeMatrix[2][3]),
            Rhodium.math.length3(nodeMatrix[3][1], nodeMatrix[3][2], nodeMatrix[3][3])
        )

        nodeMatrix = Rhodium.math.newGLTFTransform(
            translation,
            rotation,
            scale)
    else
        local translation = node.translation and vec3(node.translation) or vec3()
        local rotation = node.rotation and quaternion(node.rotation) or quaternion()
        local scale = node.scale and vec3(node.scale) or vec3(1.0)

        nodeMatrix = Rhodium.math.newGLTFTransform(
            translation,
            rotation,
            scale
        )
    end

    if node.parent then
        local parentMatrix = getNodeTransform(node.parent)

        nodeMatrix = nodeMatrix * parentMatrix
    end

    return nodeMatrix
end

local function getTransform(node)
    local nodeMatrix = getNodeTransform(node)

    -- get the scale, rotation and position from the matrix

    local scaleX = Rhodium.math.length3(nodeMatrix[1][1], nodeMatrix[1][2], nodeMatrix[1][3])
    local scaleY = Rhodium.math.length3(nodeMatrix[2][1], nodeMatrix[2][2], nodeMatrix[2][3])
    local scaleZ = Rhodium.math.length3(nodeMatrix[3][1], nodeMatrix[3][2], nodeMatrix[3][3])

    local inverseScaleX = 1.0 / scaleX
    local inverseScaleY = 1.0 / scaleY
    local inverseScaleZ = 1.0 / scaleZ

    nodeMatrix[1][1] = nodeMatrix[1][1] * inverseScaleX
    nodeMatrix[1][2] = nodeMatrix[1][2] * inverseScaleX
    nodeMatrix[1][3] = nodeMatrix[1][3] * inverseScaleX

    nodeMatrix[2][1] = nodeMatrix[2][1] * inverseScaleY
    nodeMatrix[2][2] = nodeMatrix[2][2] * inverseScaleY
    nodeMatrix[2][3] = nodeMatrix[2][3] * inverseScaleY

    nodeMatrix[3][1] = nodeMatrix[3][1] * inverseScaleZ
    nodeMatrix[3][2] = nodeMatrix[3][2] * inverseScaleZ
    nodeMatrix[3][3] = nodeMatrix[3][3] * inverseScaleZ

    local position = vec3(nodeMatrix[4][1], nodeMatrix[4][2], nodeMatrix[4][3])
    local rotation = Rhodium.math.matrixToQuaternion(nodeMatrix):normalize()
    local scale = vec3(scaleX, scaleY, scaleZ)

    -- print("Node: ", Rhodium.internal.tableToString(node))
    -- print("Position: ", position)
    -- print("Rotation: ", rotation)
    -- print("Scale: ", scale)

    return position, rotation, scale
end

local function processNodes(data)
    local meshToNode = {}
    local lightToNode = {}
    local childToParent = {}

    for i, node in ipairs(data.nodes) do
        node.index = i

        -- meshes parented to this node
        if node.mesh then
            meshToNode[node.mesh + 1] = node
        end
        if node.extensions and node.extensions.KHR_lights_punctual then
            lightToNode[node.extensions.KHR_lights_punctual.light + 1] = node
        end

        -- children of this node
        if node.children then
            for j, childIndex in ipairs(node.children) do
                childToParent[childIndex + 1] = node.index
            end
        end
    end

    for i, index in pairs(childToParent) do
        data.nodes[i].parent = data.nodes[index]
    end

    return meshToNode, lightToNode, childToParent
end

local function processMesh(meshToNode, primitive, data, meshIndex)
    local CIndicesType = getDataType(primitive.indicesTSize)

    local definition, formatName = vertexformatToFFIDefinition(primitive.vertexformat)

    fillFormatLocations(primitive.vertexformat)

    local node = meshToNode[meshIndex]

    local position, rotation, scale = getTransform(node)

    local material
    if primitive.materialIndex then
        material = data.materials[primitive.materialIndex + 1]
    end

    -- local ffiVertices = ffi.cast(formatName .. "*", primitive.vertices:getFFIPointer())

    -- local hasNormals = false
    -- local hasTangents = false

    -- for i, format in ipairs(primitive.vertexformat) do
    --     if format.name == "VertexNormal" then
    --         hasNormals = true
    --     elseif format.name == "VertexTangent" then
    --         hasTangents = true
    --     end
    -- end

    -- for i = 0, primitive.vertices:getSize() / ffi.sizeof(formatName) - 1 do
    --     local vertex = ffiVertices[i]

    --     vertex.VertexPosition.z = -vertex.VertexPosition.z

    --     if hasNormals then
    --         vertex.VertexNormal.x = -vertex.VertexNormal.x
    --         vertex.VertexNormal.y = -vertex.VertexNormal.y
    --     end

    --     if hasTangents then
    --         vertex.VertexTangent.x = -vertex.VertexTangent.x
    --         vertex.VertexTangent.y = -vertex.VertexTangent.y
    --     end
    -- end


    return {
        CIndicesType = CIndicesType,
        vertices = primitive.vertices,
        indices = primitive.indices,

        indicesTSize = primitive.indicesTSize,
        material = material,

        position = position,
        rotation = rotation,
        scale = scale,

        vertexformat = primitive.vertexformat,
        ffiVertexFormat = formatName,
        definition = definition,
    }
end

local function processMeshes(meshToNode, meshes, data)
    for meshIndex, mesh in ipairs(data.meshes) do
        for primitiveIndex, primitive in ipairs(mesh.primitives) do
            table.insert(meshes, processMesh(meshToNode, primitive, data, meshIndex))
        end
    end
end

local function processLights(lightToNode, data)
    local lights = {}

    if not data.lights then
        print("No lights found in gltf file")
        return lights
    end

    for lightIndex, light in ipairs(data.lights) do
        local node = lightToNode[lightIndex]

        local position, rotation, scale = getTransform(node)

        local self = {
            type = light.type,
            color = light.color,
            intensity = light.intensity / (4 * math.pi),
            range = light.range,
            position = position,
            rotation = rotation,
            scale = scale,
        }

        if light.spot then
            self.innerConeAngle = light.spot.innerConeAngle or 0
            self.outerConeAngle = light.spot.outerConeAngle or math.pi / 4.0
        end

        table.insert(lights, self)
    end

    return lights
end

local gltfLoader = require("Models.gltf").load

ffi.cdef [[
    typedef struct {float x, y, z, w;}  floatvec4;
    typedef struct {float x, y, z;}     floatvec3;
    typedef struct {float x, y;}        floatvec2;

    typedef struct {uint16_t x, y, z, w;} unorm16vec4;
    typedef struct {uint16_t x, y, z;}    unorm16vec3;
    typedef struct {uint16_t x, y;}       unorm16vec2;

    typedef struct {uint8_t x, y, z, w;}  uint8vec4;
    typedef struct {uint8_t x, y, z;}     uint8vec3;
    typedef struct {uint8_t x, y;}        uint8vec2;
]]

ffi.cdef [[
    typedef struct {
        floatvec3 VertexPosition;
        floatvec2 VertexTexCoord_0;
        floatvec3 VertexNormal;
        floatvec4 VertexTangent;
        unorm16vec4 VertexJoints;
        floatvec4 VertexWeights;
    } RVertexFormatAnimation;

    typedef struct {
        floatvec3 VertexPosition;
        floatvec2 VertexTexCoord_0;
        floatvec3 VertexNormal;
        floatvec4 VertexTangent;
    } RVertexFormat;
]]

--- Load a gltf file
---@param filepath string
---@return table meshes
---@return table? animation
---@return table lights
---@return table data The raw gltf data
local function loadGltfFile(filepath)
    Rhodium.internal.timesFFIVertexFormatCreated = Rhodium.internal.timesFFIVertexFormatCreated or 0

    local data = gltfLoader(filepath)
    local meshes = {}

    local animation = nil
    if #data.animations > 0 then
        animation = Rhodium.animation.loadAnimation(data)
    end

    local meshToNode, lightToNode, childToParent = processNodes(data)

    processMeshes(meshToNode, meshes, data)

    local lights = processLights(lightToNode, data)

    return meshes, animation, lights, data
end

return {
    loadGltfFile,
    processNodes = processNodes,
    processMeshes = processMeshes,
    processMesh = processMesh,
    processLights = processLights,
    getTransform = getTransform,
    getNodeTransform = getNodeTransform,
    getDataType = getDataType,
}
