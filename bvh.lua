local bvhFormat = {
    { name = "Min",           format = "floatvec3" },
    { name = "Max",           format = "floatvec3" },
    { name = "TriangleStart", format = "uint32" },
    { name = "TriangleCount", format = "uint32" },
}

BvhBuffer = newBuffer(bvhFormat, 1, { shaderstorage = true, usage = "static" })

local nodes = {}

ffi.cdef [[
    typedef struct {
        double x, y, z;
    } doublevec3;

    typedef struct {
        doublevec3 min;
        doublevec3 max;
    } bounds;

]]

---@class BvhNode
---@field bounds { min: vec3, max: vec3 }
---@field start number
---@field count number

--- Creates a new BVH node
--- @param bounds bounds
--- @param start number
--- @param count number
--- @return number
function newNode(bounds, start, count)
    local node = { bounds = bounds, start = start, count = count }

    table.insert(nodes, node)

    return #nodes
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

--- Creates a new bounds object
--- @param min? vec3
--- @param max? vec3
--- @return bounds
function newBounds(min, max)
    local minX, minY, minZ = min and min.x or math.huge, min and min.y or math.huge, min and min.z or math.huge
    local maxX, maxY, maxZ = max and max.x or -math.huge, max and max.y or -math.huge, max and max.z or -math.huge
    return ffi.new("bounds", { minX, minY, minZ }, { maxX, maxY, maxZ })
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
        local boundsLeft = newBounds()
        local boundsRight = newBounds()
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

    local indexToAxis = {
        [0] = "x",
        [1] = "y",
        [2] = "z"
    }

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
    local boundsLeft = newBounds()
    local boundsRight = newBounds()

    local sizeA = ffi.new("doublevec3")
    local sizeB = ffi.new("doublevec3")

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

--- Calculate the cost of a node
--- @param size vec3
--- @param numTriangles number
function nodeCost(size, numTriangles)
    local halfArea = size.x * size.y + size.x * size.z + size.y * size.z;
    return halfArea * numTriangles;
end

function sendBVHData()
    BvhBuffer:resize(#nodes)

    for i, node in ipairs(nodes) do
        assert(node.start, "Node triangle start is nil")
        assert(node.count, "Node triangle count is nil")

        BvhBuffer:setElementWriteIndex(i)

        BvhBuffer:write(tonumber(node.bounds.min.x) or error())
        BvhBuffer:write(tonumber(node.bounds.min.y) or error())
        BvhBuffer:write(tonumber(node.bounds.min.z) or error())

        BvhBuffer:write(tonumber(node.bounds.max.x) or error())
        BvhBuffer:write(tonumber(node.bounds.max.y) or error())
        BvhBuffer:write(tonumber(node.bounds.max.z) or error())

        BvhBuffer:write(tonumber(node.start - 1) or error())
        BvhBuffer:write(tonumber(node.count) or error())
    end

    BvhBuffer:flush()
end
