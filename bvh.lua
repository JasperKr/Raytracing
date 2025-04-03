local bvhFormat = {
    { name = "Min",           format = "floatvec3", location = 0 },
    { name = "Max",           format = "floatvec3", location = 1 },
    { name = "TriangleStart", format = "uint32",    location = 2 },
    { name = "TriangleCount", format = "uint32",    location = 3 }
}

BvhBuffer = newBuffer(bvhFormat, 1, { shaderstorage = true, usage = "static" })

local nodes

ffi.cdef [[
    typedef struct {
        float x, y, z;
    } floatvec3;

    typedef struct {
        floatvec3 min;
        floatvec3 max;
    } bounds;

    typedef struct {
        bounds* bounds;
        int64_t start;
        int64_t count;
    } node;
]]

---@class BvhNode
---@field bounds { min: vec3, max: vec3 }
---@field start number
---@field count number

do
    local nodesInit = ffi.new("node", { start = -100, count = -100 })

    local nodeCount = MAX_DEPTH ^ 5 + 32

    nodes = ffi.new("node[?]", nodeCount, nodesInit)

    local index = 0

    function getNodeCount()
        return index + 1
    end

    function getLatestNodeIndex()
        return index
    end

    function newNode(bounds, start, count)
        index = index + 1
        assert(index < nodeCount, "Ran out of BVH nodes: " .. index .. " / " .. nodeCount)
        assert(tonumber(ffi.new("int64_t", start)) == start, "Value error: " .. start)
        assert(tonumber(ffi.new("int64_t", count)) == count, "Value error: " .. count)

        nodes[index].bounds = bounds
        nodes[index].start = start
        nodes[index].count = count

        return index
    end
end

---@class triangle
---@field min vec3
---@field max vec3
---@field center vec3
---
---@field [1] number -- positions
---@field [2] number
---@field [3] number
---@field [4] number
---@field [5] number
---@field [6] number
---@field [7] number
---@field [8] number
---@field [9] number
---
---@field [10] number -- material index
---
---@field [11] number -- normal
---@field [12] number
---@field [13] number

---@class bounds
---@field min vec3
---@field max vec3

local tempMin = ffi.new("floatvec3")
local tempMax = ffi.new("floatvec3")

--- Creates a new bounds object
--- @param min? vec3
--- @param max? vec3
--- @return bounds
function newBounds(min, max)
    tempMin.x, tempMin.y, tempMin.z = min.x, min.y, min.z
    tempMax.x, tempMax.y, tempMax.z = max.x, max.y, max.z
    return ffi.new("bounds", tempMin, tempMax)
end

local tempMinHuge = ffi.new("floatvec3", math.huge, math.huge, math.huge)
local tempMaxHuge = ffi.new("floatvec3", -math.huge, -math.huge, -math.huge)

local boundsInit = ffi.new("bounds", tempMinHuge, tempMaxHuge)

local bounds = {}

local count = MAX_DEPTH
table.insert(bounds, {
    data = ffi.new("bounds[?]", count, boundsInit),
    index = count
})
local total = count
local current = bounds[1]

function emptyBounds()
    if current.index == 0 then
        table.insert(bounds, {
            data = ffi.new("bounds[?]", total, boundsInit),
            index = total
        })
        current = bounds[#bounds]
        total = total * 2
    end

    current.index = current.index - 1
    return current.data[current.index]
end

do
    local min, max = math.min, math.max

    ---@param triangle triangle
    function include(self, triangle)
        self.min.x = min(self.min.x, triangle.min.x)
        self.min.y = min(self.min.y, triangle.min.y)
        self.min.z = min(self.min.z, triangle.min.z)

        self.max.x = max(self.max.x, triangle.max.x)
        self.max.y = max(self.max.y, triangle.max.y)
        self.max.z = max(self.max.z, triangle.max.z)
    end
end

local bvhTreeMetatable = {}
local bvhTreeFunctions = {}
bvhTreeMetatable.__index = bvhTreeFunctions

local maxDepthReached = 0

--- Creates a new BVH tree
---@return table
function newBvhTree(triangles)
    maxDepthReached = 0

    local self = {
        triangles = triangles,
    }

    print("Preparing triangle data...")
    local t = love.timer.getTime()

    local min, max = vec3(math.huge), vec3(-math.huge)

    for i, triangle in ipairs(triangles) do
        local x1, y1, z1 = triangle[1], triangle[2], triangle[3]
        local x2, y2, z2 = triangle[4], triangle[5], triangle[6]
        local x3, y3, z3 = triangle[7], triangle[8], triangle[9]

        triangle.center = vec3(x1 + x2 + x3, y1 + y2 + y3, z1 + z2 + z3) / 3
        triangle.min = vec3(math.min(x1, x2, x3), math.min(y1, y2, y3), math.min(z1, z2, z3))
        triangle.max = vec3(math.max(x1, x2, x3), math.max(y1, y2, y3), math.max(z1, z2, z3))

        min:min(triangle.min)
        max:max(triangle.max)
    end

    print("Triangle data prepared in ", love.timer.getTime() - t, "s")

    newNode(newBounds(min, max), 1, 0)

    splitTree(1, triangles, 1, #triangles, 0)

    print("Max depth reached: ", maxDepthReached)

    setmetatable(self, bvhTreeMetatable)

    return self
end

local outSize = vec3(0, 0, 0)

--- Calculate the cost of a node
--- @param size vec3
--- @param numTriangles number
local function nodeCost(size, numTriangles)
    local halfArea = size.x * size.y + size.x * size.z + size.y * size.z;
    return halfArea * numTriangles;
end

--- Builds the BVH tree
---@param parentIndex number
---@param triangles table
---@param triGlobalStart number
---@param triNum number
---@param depth number
function splitTree(parentIndex, triangles, triGlobalStart, triNum, depth)
    maxDepthReached = math.max(maxDepthReached, depth)

    local parent = nodes[parentIndex]

    mathv.sub3(parent.bounds.max, parent.bounds.min, outSize)
    local parentCost = nodeCost(outSize, triNum);

    local splitAxis, splitPos, cost = chooseSplit(triangles, parent, triGlobalStart, triNum);

    if cost < parentCost and depth < MAX_DEPTH then
        local boundsLeft = emptyBounds()
        local boundsRight = emptyBounds()
        local numOnLeft = 0;

        for i = triGlobalStart, triGlobalStart + triNum - 1 do
            local triangle = triangles[i]

            if triangle.center[splitAxis] < splitPos then
                include(boundsLeft, triangle)

                swap = triangles[triGlobalStart + numOnLeft];
                triangles[triGlobalStart + numOnLeft] = triangle;
                triangles[i] = swap;
                numOnLeft = numOnLeft + 1;
            else
                include(boundsRight, triangle)
            end
        end

        local numOnRight = triNum - numOnLeft;
        local triStartLeft = triGlobalStart;
        local triStartRight = triGlobalStart + numOnLeft;

        local childIndexLeft = newNode(boundsLeft, triStartLeft, 0)
        local childIndexRight = newNode(boundsRight, triStartRight, 0)

        parent.start = childIndexLeft;

        splitTree(childIndexLeft, triangles, triGlobalStart, numOnLeft, depth + 1)
        splitTree(childIndexRight, triangles, triGlobalStart + numOnLeft, numOnRight, depth + 1)
    else
        parent.start = triGlobalStart;
        parent.count = triNum;
    end
end

local function mix(a, b, i)
    return a * (1 - i) + b * i
end

local indexToAxis = {
    [0] = "x",
    [1] = "y",
    [2] = "z"
}

--- choose the best split position for the given triangles
---@param triangles table
---@param node BvhNode
---@param start any
---@param count any
---@return "x"|"y"|"z"
---@return integer
---@return number
function chooseSplit(triangles, node, start, count)
    if count <= 1 then return "x", 0, math.huge end

    local bestSplitPos = 0;
    local bestSplitAxis = "x";
    local numSplitTests = 5;

    local bestCost = math.huge

    -- Estimate best split pos
    for axisIndex = 0, 2 do
        axis = indexToAxis[axisIndex]

        for i = 1, numSplitTests do
            local splitT = i / (numSplitTests + 1);
            local splitPos = mix(node.bounds.min[axis], node.bounds.max[axis], splitT);
            local cost = evaluateSplit(triangles, axis, splitPos, start, count);

            if cost < bestCost then
                bestCost = cost;
                bestSplitPos = splitPos;
                bestSplitAxis = axis;
            end
        end
    end

    return bestSplitAxis, bestSplitPos, bestCost
end

do
    local boundsLeft = emptyBounds()
    local boundsRight = emptyBounds()

    local sizeA = ffi.new("floatvec3")
    local sizeB = ffi.new("floatvec3")

    --- Evaluate the cost of a split
    ---@param triangles table
    ---@param splitAxis "x"|"y"|"z"
    ---@param splitPos number
    ---@param start number
    ---@param count number
    function evaluateSplit(triangles, splitAxis, splitPos, start, count)
        local numOnLeft = 0
        local numOnRight = 0

        boundsLeft.min.x, boundsLeft.min.y, boundsLeft.min.z = math.huge, math.huge, math.huge
        boundsLeft.max.x, boundsLeft.max.y, boundsLeft.max.z = -math.huge, -math.huge, -math.huge

        boundsRight.min.x, boundsRight.min.y, boundsRight.min.z = math.huge, math.huge, math.huge
        boundsRight.max.x, boundsRight.max.y, boundsRight.max.z = -math.huge, -math.huge, -math.huge

        for i = start, start + count - 1 do
            local tri = triangles[i]

            if (tri.center[splitAxis] < splitPos) then
                include(boundsLeft, tri)
                numOnLeft = numOnLeft + 1
            else
                include(boundsRight, tri)
                numOnRight = numOnRight + 1
            end
        end

        mathv.sub3(boundsLeft.max, boundsLeft.min, sizeA)
        mathv.sub3(boundsRight.max, boundsRight.min, sizeB)

        local costA = nodeCost(sizeA, numOnLeft)
        local costB = nodeCost(sizeB, numOnRight)
        return costA + costB;
    end
end

function sendBVHData()
    BvhBuffer:resize(getNodeCount())

    print("Sending BVH data...: " .. getNodeCount())

    for i = 1, getLatestNodeIndex() do
        local node = nodes[i]

        BvhBuffer:setElementWriteIndex(i)

        BvhBuffer:write(node.bounds.min.x)
        BvhBuffer:write(node.bounds.min.y)
        BvhBuffer:write(node.bounds.min.z)

        BvhBuffer:write(node.bounds.max.x)
        BvhBuffer:write(node.bounds.max.y)
        BvhBuffer:write(node.bounds.max.z)

        BvhBuffer:writeUIntMany(node.start - 1, node.count)
    end

    BvhBuffer:flush()
end
