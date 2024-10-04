ffi = require("ffi")

do
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

    Rhodium.internal.formatToLocation = {
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

    function Rhodium.internal.fillFormatLocations(format)
        for i, v in pairs(format) do
            format[i].location = Rhodium.internal.formatToLocation[v.name]
        end
    end

    local timesFFIVertexFormatCreated = 0
    local formats = {}

    function Rhodium.internal.vertexformatToFFIDefinition(format)
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
end

local loaderFunctions = require("Rhodium.Modules.Models.loader")

local data, meshIndex, primitiveIndex = ...

local meshToNode, lightToNode, childToParent = loaderFunctions.processNodes(data)

local meshData = data.meshes[meshIndex]
local primitive = meshData.primitives[primitiveIndex]

local mesh = loaderFunctions.processMesh(meshToNode, primitive, data, meshIndex)

mesh.position = mesh.position:table()
mesh.rotation = mesh.rotation:table()
mesh.scale = mesh.scale:table()

return mesh
