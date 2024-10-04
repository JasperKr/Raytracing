--[[

buffer wrapper, each buffer is a list of elements,
the format describes how the elements are structured
each item in the format describes an attribute of the element
and each attribute has one or more components:

format = {
    { name = "Position", format = "floatvec3" },
    { name = "Color", format = "floatvec4" }
} --->

{ x, y, z, r, g, b, a, ... }

[x, y, z], [r, g, b, a] are the attributes
x, y, ... are the the components
[x, y, z, r, g, b, a] is one element

]]

local bufferMetatable = {}

---@class buffer
---@field buffer love.GraphicsBuffer the buffer object
---@field componentCount integer the amount of components in an element
---@field data love.ByteData the data of the buffer
---@field updated boolean whether the data has been changed and needs to be sent to the GPU on flush
---@field format table the format of the buffer
---@field formatIndexTable { string:integer } a table of component names to their index in the buffer
---@field engineData table a table for engine data
---@field writeIndex integer the current component write index
---@field elementCount integer the amount of elements in the buffer
---@field dataReferences { float:ffi.cdata*, int32_t:ffi.cdata*, uint32_t:ffi.cdata* } the data references for each component type
---@field componentTypes { [1]:"float"|"int32_t"|"uint32_t" } the component types in the buffer
---@field componentIndexToArrayIndex { [1]:integer } the index of the component in the buffer array
---@field byteDataPtr ffi.cdata* the pointer to the byte data
---@field elementComponentStride integer the stride of an element in components, since the final stride can be different due to internal padding
---@field invComponentCount number the inverse of the component count
---@field bufferCreationSettings table the settings used to create the buffer object
local bufferFunctions = {}
bufferMetatable.__index = bufferFunctions

--- the amount of components in an attribute
local attributeComponentCount = {
    float = 1,
    floatvec2 = 2,
    floatvec3 = 3,
    floatvec4 = 4,
    floatmat2x2 = 4,
    floatmat2x3 = 6,
    floatmat2x4 = 8,
    floatmat3x2 = 6,
    floatmat3x3 = 9,
    floatmat3x4 = 12,
    floatmat4x2 = 8,
    floatmat4x3 = 12,
    floatmat4x4 = 16,
    int32 = 1,
    int32vec2 = 2,
    int32vec3 = 3,
    int32vec4 = 4,
    uint32 = 1,
    uint32vec2 = 2,
    uint32vec3 = 3,
    uint32vec4 = 4
}

--- the ffi types for each buffer component type
local ffiTypes = {
    float       = "float",
    floatvec2   = "float",
    floatvec3   = "float",
    floatvec4   = "float",
    floatmat2x2 = "float",
    floatmat2x3 = "float",
    floatmat2x4 = "float",
    floatmat3x2 = "float",
    floatmat3x3 = "float",
    floatmat3x4 = "float",
    floatmat4x2 = "float",
    floatmat4x3 = "float",
    floatmat4x4 = "float",
    int32       = "int32_t",
    int32vec2   = "int32_t",
    int32vec3   = "int32_t",
    int32vec4   = "int32_t",
    uint32      = "uint32_t",
    uint32vec2  = "uint32_t",
    uint32vec3  = "uint32_t",
    uint32vec4  = "uint32_t"
}

--- Wrapper for love.graphics.newBuffer
---@param format table
---@param elementCount number
---@param settings table
---@return buffer
function newBuffer(format, elementCount, settings)
    assert(elementCount > 0, "Buffer item count must be greater than 0")

    local buffer = love.graphics.newBuffer(format, elementCount, settings)

    local byteData = love.data.newByteData(buffer:getSize())
    local byteDataPtr = byteData:getFFIPointer()

    local floatArray = ffi.cast("float*", byteDataPtr)
    local int32Array = ffi.cast("int32_t*", byteDataPtr)
    local uint32Array = ffi.cast("uint32_t*", byteDataPtr)

    local componentTypes = {}

    local index = 0

    for i, component in ipairs(format) do
        local componentType = ffiTypes[component.format]

        assert(componentType, "Invalid buffer component format: " .. component.format)

        for j = 1, attributeComponentCount[component.format] do
            table.insert(componentTypes, componentType)

            if componentType == "float" then
                floatArray[index] = 0
            elseif componentType == "int32_t" then
                int32Array[index] = 0
            elseif componentType == "uint32_t" then
                uint32Array[index] = 0
            end

            index = index + 1
        end
    end

    local dataFormat = buffer:getFormat()
    local formatIndexTable = {}
    local componentIndexToArrayIndex = {}

    local componentCount = index
    local componentIndex = 0

    index = 0

    local hasIndexTable = true
    for i, component in ipairs(dataFormat) do
        if component.name then
            formatIndexTable[component.name] = componentIndex
        else
            hasIndexTable = false
        end

        for j = 1, attributeComponentCount[component.format] do
            componentIndexToArrayIndex[componentIndex] = component.offset / 4 + j - 1

            componentIndex = componentIndex + 1
        end

        index = index + 1
    end

    return setmetatable({
        buffer = buffer,
        data = byteData,

        dataFormat = dataFormat,
        byteDataPtr = byteDataPtr,

        dataReferences = {
            float = floatArray,
            int32_t = int32Array,
            uint32_t = uint32Array
        },

        componentTypes = componentTypes,
        componentIndexToArrayIndex = componentIndexToArrayIndex,

        updated = true,
        format = format,

        formatIndexTable = hasIndexTable and formatIndexTable or nil, -- only set if all components have names
        engineData = {},

        writeIndex = 0,
        elementCount = elementCount,

        componentCount = componentCount,

        elementComponentStride = buffer:getElementStride() / 4,
        invComponentCount = 1.0 / componentCount,

        bufferCreationSettings = settings
    }, bufferMetatable)
end

--- resizes the buffer
--- @param count integer
--- @return love.GraphicsBuffer
function bufferFunctions:resize(count)
    if self.elementCount == count then
        return self.buffer -- no need to resize
    end

    self.elementCount = count

    local buffer = love.graphics.newBuffer(self.format, count, self.bufferCreationSettings)

    local byteData = love.data.newByteData(buffer:getSize())
    local byteDataPtr = byteData:getFFIPointer()

    local floatArray = ffi.cast("float*", byteDataPtr)
    local int32Array = ffi.cast("int32_t*", byteDataPtr)
    local uint32Array = ffi.cast("uint32_t*", byteDataPtr)

    ffi.copy(byteDataPtr, self.byteDataPtr, math.min(self.data:getSize(), byteData:getSize()))

    self.data:release()
    self.buffer:release()

    self.buffer = buffer
    self.data = byteData
    self.byteDataPtr = byteDataPtr

    self.dataReferences = {
        float = floatArray,
        int32_t = int32Array,
        uint32_t = uint32Array
    }

    self.updated = true

    self:flush()

    return buffer
end

--- gets the löve buffer object of the buffer
--- @return love.GraphicsBuffer
function bufferFunctions:getBuffer()
    return self.buffer
end

--- gets the data of the buffer
--- @return love.ByteData
function bufferFunctions:getData()
    return self.data
end

function bufferFunctions:getComponentType(index)
    local typeIndex = index % self.componentCount + 1

    return self.componentTypes[typeIndex]
end

function bufferFunctions:getArrayIndex(index)
    local arrayIndex = self.componentIndexToArrayIndex[index % self.componentCount]
    arrayIndex = arrayIndex + math.floor(index * self.invComponentCount + 0.001) * self.elementComponentStride

    return arrayIndex
end

--- sets a number at a specific index in the buffer
---@param index integer
---@param value number
function bufferFunctions:setNumberAt(index, value)
    local arrayIndex = self:getArrayIndex(index)

    self.dataReferences.float[arrayIndex] = value
end

--- sets a int at a specific index in the buffer
--- @param index integer
--- @param value number
function bufferFunctions:setIntAt(index, value)
    local arrayIndex = self:getArrayIndex(index)

    self.dataReferences.int32_t[arrayIndex] = value
end

--- sets a uint at a specific index in the buffer
--- @param index integer
--- @param value number
function bufferFunctions:setUintAt(index, value)
    local arrayIndex = self:getArrayIndex(index)

    self.dataReferences.uint32_t[arrayIndex] = value
end

local acceptedTypes = {
    ["vec2"] = true,
    ["vec3"] = true,
    ["vec4"] = true,
    ["mat3"] = true,
    ["mat4"] = true,
    ["quaternion"] = true,
    ["physicsBody"] = true,
    ["physicsShape"] = true,
    ["spot"] = true,
    ["directional"] = true,
    ["point"] = true,
    ["area"] = true,
    ["volume"] = true,
    ["sphere"] = true,
    ["uiConstraintPoint"] = true,
    ["uiConstraintAxisPoint"] = true,
    ["lightProbe"] = true,
    ["scene"] = true,
}

Rtypes = { -- cache types so we dont have to call vec4().CType every time
    vec4 = vec4().CType,
    vec3 = vec3().CType,
    vec2 = vec2().CType,
    quaternion = quaternion().CType,
}

--- returns the type of a variable
---@param x any
---@return "vec2"|"vec3"|"vec4"|"quaternion"|"physicsBody"|"physicsShape"|"unknown"|"spot"|"directional"|"point"|"area"|"volume"|"sphere"|"uiConstraintPoint"|"uiConstraintAxisPoint"|"lightProbe"|"mat3"|"mat4"|"scene"
function Rtype(x)
    local t = type(x)
    if t == "cdata" then
        local ct = ffi.typeof(x)

        if ct == Rtypes.vec2 then
            return "vec2"
        elseif ct == Rtypes.vec3 then
            return "vec3"
        elseif ct == Rtypes.vec4 then
            return "vec4"
        elseif ct == Rtypes.quaternion then
            return "quaternion"
        else
            return "unknown"
        end
    elseif t == "table" then
        if acceptedTypes[x.type] then
            return x.type
        elseif x.type == "AABB light probe" or x.type == "Sphere light probe" or x.type == "Infinite light probe" then
            return "lightProbe"
        else
            return "unknown"
        end
    else
        return "unknown"
    end
end

--- sets a value at a specific index in the buffer
--- @param index integer
--- @param value number|vec2|vec3|vec4|quaternion|matrix3x3|matrix4x4|table
--- @return integer # the new index, incremented by the amount of components written
function bufferFunctions:setAt(index, value)
    local t = type(value)
    local RType = Rtype(value)

    assert(index >= 0)

    if t == "number" then
        local array = self.dataReferences[self:getComponentType(index)]

        local arrayIndex = self:getArrayIndex(index)

        array[arrayIndex] = value
        self.updated = true

        return index + 1
    elseif RType == "vec2" then
        index = self:setAt(index, value.x)
        index = self:setAt(index, value.y)
    elseif RType == "vec3" then
        index = self:setAt(index, value.x)
        index = self:setAt(index, value.y)
        index = self:setAt(index, value.z)
    elseif RType == "vec4" then
        index = self:setAt(index, value.x)
        index = self:setAt(index, value.y)
        index = self:setAt(index, value.z)
        index = self:setAt(index, value.w)
    elseif RType == "quat" then
        index = self:setAt(index, value.x)
        index = self:setAt(index, value.y)
        index = self:setAt(index, value.z)
        index = self:setAt(index, value.w)
    elseif RType == "mat3" then
        index = self:setAt(index, value[1][1])
        index = self:setAt(index, value[1][2])
        index = self:setAt(index, value[1][3])
        index = self:setAt(index, value[2][1])
        index = self:setAt(index, value[2][2])
        index = self:setAt(index, value[2][3])
        index = self:setAt(index, value[3][1])
        index = self:setAt(index, value[3][2])
        index = self:setAt(index, value[3][3])
    elseif RType == "mat4" then
        index = self:setAt(index, value[1][1])
        index = self:setAt(index, value[1][2])
        index = self:setAt(index, value[1][3])
        index = self:setAt(index, value[1][4])
        index = self:setAt(index, value[2][1])
        index = self:setAt(index, value[2][2])
        index = self:setAt(index, value[2][3])
        index = self:setAt(index, value[2][4])
        index = self:setAt(index, value[3][1])
        index = self:setAt(index, value[3][2])
        index = self:setAt(index, value[3][3])
        index = self:setAt(index, value[3][4])
        index = self:setAt(index, value[4][1])
        index = self:setAt(index, value[4][2])
        index = self:setAt(index, value[4][3])
        index = self:setAt(index, value[4][4])
    elseif t == "table" then
        for i, v in ipairs(value) do
            index = self:setAt(index, v)
        end
    else
        print(type(value), RType)
        error("Engine Error: Invalid buffer write value type")
    end

    return index
end

--- writes to the buffer iteratively,
--- can be used in a for-loop to write all the data
--- @param value number|vec2|vec3|vec4|quaternion|matrix3x3|matrix4x4|table
function bufferFunctions:write(value)
    self.writeIndex = self:setAt(self.writeIndex, value)

    return self
end

--- writes to the buffer iteratively,
--- can be used in a for-loop to write all the data
--- @param value number float value
function bufferFunctions:writeFloat(value)
    local arrayIndex = self:getArrayIndex(self.writeIndex)

    self.dataReferences.float[arrayIndex] = value

    self.writeIndex = self.writeIndex + 1

    return self
end

--- writes to the buffer iteratively,
--- can be used in a for-loop to write all the data
--- @param value number int value
function bufferFunctions:writeInt(value)
    local arrayIndex = self:getArrayIndex(self.writeIndex)

    self.dataReferences.int32_t[arrayIndex] = value

    self.writeIndex = self.writeIndex + 1

    return self
end

--- writes to the buffer iteratively,
--- can be used in a for-loop to write all the data
--- @param value number uint value
function bufferFunctions:writeUint(value)
    local arrayIndex = self:getArrayIndex(self.writeIndex)

    self.dataReferences.uint32_t[arrayIndex] = value

    self.writeIndex = self.writeIndex + 1

    return self
end

--- sets the component write index of the buffer, 0-based indexing
--- @param index integer
function bufferFunctions:setComponentWriteIndex(index)
    self.writeIndex = index
    assert(self.writeIndex >= 0, "Buffer write index out of bounds")

    return self
end

--- gets the component write index of the buffer
--- @return integer
function bufferFunctions:getWriteIndex()
    return self.writeIndex
end

--- gets the item write index of the buffer
--- @return integer
function bufferFunctions:getElementWriteIndex()
    return math.floor(self.writeIndex / self.componentCount + 0.001)
end

--- sets the write index of the buffer to the start of an item
function bufferFunctions:setElementWriteIndex(index)
    self.writeIndex = (index - 1) * self.componentCount
end

--- updates the buffer with the data and sends it to the GPU
function bufferFunctions:flush()
    if self.updated then
        self.buffer:setArrayData(self.data)
        self.updated = false
    end

    return self
end

--- adds 1 to the write index
function bufferFunctions:skipComponent()
    self.writeIndex = self.writeIndex + 1

    return self
end

--- adds a number to the write index
function bufferFunctions:skipComponents(count)
    self.writeIndex = self.writeIndex + count

    return self
end

--- copies an item from the buffer to another item
---@param source number
---@param destination number
function bufferFunctions:copyElementTo(source, destination)
    local sourceIndex = source * self.componentCount
    local destinationIndex = destination * self.componentCount

    for i = 0, self.componentCount - 1 do
        local array = self.dataReferences[self:getComponentType(i)]

        local destComponentIndex = self:getArrayIndex(destinationIndex + i)
        local sourceComponentIndex = self:getArrayIndex(sourceIndex + i)

        array[destComponentIndex] = array[sourceComponentIndex]
    end
    self.updated = true

    return self
end

--- gets the total size of an element in the buffer
--- @return integer
function bufferFunctions:getElementComponentSize()
    return self.componentCount
end

--- formats a number to a string
--- @param itemType "float"|"int32_t"|"uint32_t"
--- @param num number
--- @return string
local formatNum = function(itemType, num)
    num = num or "INVALID VALUE"

    if itemType == "float" then
        return string.format("%.4f", num)
    elseif itemType == "int32_t" then
        return num .. "i"
    elseif itemType == "uint32_t" then
        return num .. "ui"
    end

    error("Engine error: Invalid item type")
end

--- Sets a buffer component value.
--- Does not increment the write index
--- @param name string
--- @param value number|vec2|vec3|vec4|quaternion|matrix3x3|matrix4x4|table
--- @param offset? integer
function bufferFunctions:set(name, value, offset)
    assert(self.formatIndexTable, "One or more buffer components do not have names")
    local index = self.formatIndexTable[name]

    assert(index, "Invalid buffer component name: " .. name)

    index = index + (offset or 0) * self.componentCount

    return self:setAt(index, value)
end
