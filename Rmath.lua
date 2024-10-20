PI2 = math.pi * 2
IPI2 = 1 / PI2
IPI = 1 / math.pi
PI05 = math.pi * 0.5
IPI05 = 1 / PI05

function Rhodium.math.clamp(v, min, max)
    return math.min(math.max(v, min), max)
end

function Rhodium.math.round(x, i)
    i = i or 0
    i = 10 ^ i
    return math.floor(x * i + 0.5) / i
end

function Rhodium.math.saturate(x)
    return math.min(math.max(x, 0.0), 1.0)
end

local function convertMantissa(i)
    local m = bit.lshift(i, 13)                 -- Zero pad mantissa bits
    local e = ffi.new("uint32_t", 0)            -- Zero exponent

    while (not bit.band(m, 0x00800000)) do      -- While not normalized
        e = e - ffi.new("uint32_t", 0x00800000) -- Decrement exponent (1<<23)
        m = bit.lshift(m, 1)                    -- Shift mantissa
    end

    m = bit.band(m, bit.bnot(0x00800000))   -- Clear leading 1 bit
    e = e + ffi.new("uint32_t", 0x38800000) -- Adjust bias ((127-14)<<23)

    return bit.bor(m, e)                    -- Return combined number
end

local mantissatable = ffi.new("uint32_t[2048]");
local offsettable = ffi.new("uint16_t[64]");
local exponenttable = ffi.new("uint32_t[64]");

-- tables for float -> half conversions
local basetable = ffi.new("uint16_t[512]");
local shifttable = ffi.new("uint8_t[512]");

do
    -- tables for float16 -> float32 conversions.

    mantissatable[0] = 0

    for i = 1, 1024 - 1 do
        mantissatable[i] = convertMantissa(i)
    end

    for i = 1024, 2048 - 1 do
        mantissatable[i] = 0x38000000 + bit.lshift((i - 1024), 13)
    end

    exponenttable[0] = 0
    exponenttable[32] = 0x80000000

    for i = 0, 31 - 1 do
        exponenttable[i] = bit.lshift(i, 23)
    end

    for i = 33, 63 - 1 do
        exponenttable[i] = 0x80000000 + bit.lshift((i - 32), 23)
    end

    exponenttable[31] = 0x47800000
    exponenttable[63] = 0xC7800000

    for i = 0, 64 - 1 do
        if (i == 0 or i == 32) then
            offsettable[i] = 0
        else
            offsettable[i] = 1024
        end
    end


    -- tables for float32 -> float16 conversions.

    for i = 0, 256 - 1 do
        local e = i - 127

        if (e < -24) then -- Very small numbers map to zero
            basetable[bit.bor(i, 0x000)] = 0x0000
            basetable[bit.bor(i, 0x100)] = 0x8000
            shifttable[bit.bor(i, 0x000)] = 24
            shifttable[bit.bor(i, 0x100)] = 24
        elseif e < -14 then -- Small numbers map to denorms
            basetable[bit.bor(i, 0x000)] = bit.rshift(0x0400, (-e - 14))
            basetable[bit.bor(i, 0x100)] = bit.bor(bit.rshift(0x0400, (-e - 14)), 0x8000)
            shifttable[bit.bor(i, 0x000)] = -e - 1
            shifttable[bit.bor(i, 0x100)] = -e - 1
        elseif e <= 15 then -- Normal numbers just lose precision
            basetable[bit.bor(i, 0x000)] = bit.lshift((e + 15), 10)
            basetable[bit.bor(i, 0x100)] = bit.bor(bit.lshift((e + 15), 10), 0x8000)
            shifttable[bit.bor(i, 0x000)] = 13
            shifttable[bit.bor(i, 0x100)] = 13
        elseif e < 128 then -- Large numbers map to Infinity
            basetable[bit.bor(i, 0x000)] = 0x7C00
            basetable[bit.bor(i, 0x100)] = 0xFC00
            shifttable[bit.bor(i, 0x000)] = 24
            shifttable[bit.bor(i, 0x100)] = 24
        else -- Infinity and NaN's stay Infinity and NaN's
            basetable[bit.bor(i, 0x000)] = 0x7C00
            basetable[bit.bor(i, 0x100)] = 0xFC00
            shifttable[bit.bor(i, 0x000)] = 13
            shifttable[bit.bor(i, 0x100)] = 13
        end
    end
end

do
    local tempFloat = ffi.new("float[1]")
    local tempUInt = ffi.new("uint32_t[1]")
    local tempUint16 = ffi.new("uint16_t[1]")

    --https://github.com/love2d/love/blob/1ecc8a1fd9ae8327525bafcbceeb48cdc9bf4fd1/src/common/floattypes.cpp#L162
    function Rhodium.math.float32to16(f)
        tempFloat[0] = f
        f = ffi.cast("float*", tempFloat[0])
        tempUInt[0] = ffi.cast("uint32_t", ffi.cast("uint32_t*", f))
        tempUint16[0] = basetable[bit.band(bit.rshift(tempUInt[0], 23), 0x1FF)] +
            bit.rshift(bit.band(tempUInt[0], 0x007FFFFF), shifttable[bit.band(bit.rshift(tempUInt[0], 23), 0x1FF)])

        return tempUint16[0]
    end
end

do
    local float32 = ffi.new("float[1]")
    local scratch_0 = ffi.new("uint32_t", 0x007FFFFF)
    local scratch_1 = ffi.new("uint32_t", 23)
    local scratch_2 = ffi.new("uint32_t", 0x1FF)

    --https://github.com/love2d/love/blob/1ecc8a1fd9ae8327525bafcbceeb48cdc9bf4fd1/src/common/floattypes.cpp#L162
    function Rhodium.math.float32to16uint32(f)
        float32[0] = f
        local data = ffi.cast("uint32_t *", float32)
        local index = bit.band(bit.rshift(data[0], scratch_1), scratch_2)

        return basetable[index] + bit.rshift(bit.band(data[0], scratch_0), shifttable[index])
    end
end

---@alias Rhodium.meshMaterial {normalMap:love.Texture,albedoMap:love.Texture,roughnessMap:love.Texture,metallicMap:love.Texture,environmentMap:love.Texture,meshCullMode:love.CullMode,meshes:table}

--- creates vertices for a box
---@param w number
---@param h number
---@param d number
---@param x? number
---@param y? number
---@param z? number
---@return table
function Rhodium.internal.boxVerticesFromSize(w, h, d, x, y, z, vertices, createUvs, createNormals)
    x = x or 0
    y = y or 0
    z = z or 0
    local verts = {
        { 1,  1,  -1 },
        { 1,  -1, -1 },
        { 1,  1,  1 },
        { 1,  -1, 1 },
        { -1, 1,  -1 },
        { -1, -1, -1 },
        { -1, 1,  1 },
        { -1, -1, 1 } }
    local texCoords = {
        { 0.625, 0.5 },
        { 0.375, 0.5 },
        { 0.625, 0.75 },
        { 0.375, 0.75 },
        { 0.875, 0.5 },
        { 0.625, 0.25 },
        { 0.125, 0.5 },
        { 0.375, 0.25 },
        { 0.875, 0.75 },
        { 0.625, 1 },
        { 0.625, 0 },
        { 0.375, 0 },
        { 0.375, 1 },
        { 0.125, 0.75 } }

    local triangles = {
        { { 5, 5, 1 },  { 3, 3, 1 },  { 1, 1, 1 } },
        { { 3, 3, 2 },  { 8, 13, 2 }, { 4, 4, 2 } },
        { { 7, 11, 3 }, { 6, 8, 3 },  { 8, 12, 3 } },
        { { 2, 2, 4 },  { 8, 14, 4 }, { 6, 7, 4 } },
        { { 1, 1, 5 },  { 4, 4, 5 },  { 2, 2, 5 } },
        { { 5, 6, 6 },  { 2, 2, 6 },  { 6, 8, 6 } },
        { { 5, 5, 1 },  { 7, 9, 1 },  { 3, 3, 1 } },
        { { 3, 3, 2 },  { 7, 10, 2 }, { 8, 13, 2 } },
        { { 7, 11, 3 }, { 5, 6, 3 },  { 6, 8, 3 } },
        { { 2, 2, 4 },  { 4, 4, 4 },  { 8, 14, 4 } },
        { { 1, 1, 5 },  { 3, 3, 5 },  { 4, 4, 5 } },
        { { 5, 6, 6 },  { 1, 1, 6 },  { 2, 2, 6 } },
    }

    local vertices = vertices or {}
    for i, v in ipairs(triangles) do
        for j = 1, 3 do
            local vert = v[j]
            local pos = verts[vert[1]]
            local tx = texCoords[vert[2]]
            table.insert(vertices,
                { pos[1] * w * 0.5 + x, pos[2] * h * 0.5 + y, pos[3] * d * 0.5 + z, tx[1], tx[2], 0, 1, 0 })
        end
    end
    return vertices
end

function Rhodium.math.AABB(b1, b2)
    local v, w = b1:getBoundingBox(), b2:getBoundingBox()
    return v[1] + v[4] > w[1] and v[2] + v[5] > w[2] and v[1] < w[1] + w[4] and v[2] < w[2] + w[5] and
        v[3] + v[6] > w[3] and v[3] < w[3] + w[6]
end

local function printTableInternal(t, floor, names, loopedTables)
    io.write(("  "):rep(#names) .. (names[#names] and names[#names] .. ": " or "") .. "{\n")
    for i, v in pairs(t) do
        if type(v) == "table" then
            if loopedTables[v] then
                io.write(("  "):rep(#names + 1) .. i .. " = Reference to " .. loopedTables[v] .. "\n")
                goto continue
            end
            if v == t or v == _G then
                io.write(("  "):rep(#names + 1) .. i .. " = Reference to self\n")
                goto continue
            end
            if not next(v) then
                io.write(("  "):rep(#names + 1) .. i .. " = {}\n")
                goto continue
            end
            table.insert(names, i)
            loopedTables[v] = table.concat(names, ".")
            printTableInternal(v, floor, names, loopedTables)
            table.remove(names, #names)
        else
            if floor then
                if type(v) == "number" then
                    io.write(("  "):rep(#names + 1) .. i .. " = " .. Rhodium.math.round(v) .. "\n")
                else
                    io.write(("  "):rep(#names + 1) .. i .. " = " .. v .. "\n")
                end
            else
                io.write(("  "):rep(#names + 1) .. tostring(i) .. " = " .. tostring(v) .. "\n")
            end
        end
        ::continue::
    end
    io.write(("  "):rep(#names) .. "}\n")
end

local function TableToStringInternal(t, finalString, names, loopedTables)
    finalString = finalString .. ("  "):rep(#names) .. (names[#names] and names[#names] .. ": " or "") .. "{\n"
    for i, v in pairs(t) do
        if type(v) == "table" then
            if loopedTables[v] then
                finalString = finalString ..
                    ("  "):rep(#names + 1) .. tostring(i) .. " = Reference to " .. loopedTables[v] .. "\n"
                goto continue
            end
            if v == t or v == _G then
                finalString = finalString .. ("  "):rep(#names + 1) .. tostring(i) .. " = Reference to self\n"
                goto continue
            end
            if not next(v) then
                finalString = finalString .. ("  "):rep(#names + 1) .. tostring(i) .. " = {}\n"
                goto continue
            end
            table.insert(names, i)
            loopedTables[v] = table.concat(names, ".")
            finalString = TableToStringInternal(v, finalString, names, loopedTables)
            table.remove(names, #names)
        else
            if type(v) == "userdata" or type(v) == "function" or type(v) == "thread" then
                finalString = finalString .. ("  "):rep(#names + 1) .. tostring(i) .. " = " .. type(v) .. "\n"
            else
                finalString = finalString .. ("  "):rep(#names + 1) .. tostring(i) .. " = " .. tostring(v) .. "\n"
            end
        end
        ::continue::
    end
    finalString = finalString .. ("  "):rep(#names) .. "}\n"
    return finalString
end

function Rhodium.internal.tableToString(t)
    local names = {}
    if type(t) == "table" then
        return TableToStringInternal(t, "", names, {})
    else
        return tostring(t)
    end
end

function Rhodium.internal.printTable(t, floor)
    local names = {}
    if type(t) == "table" then
        printTableInternal(t, floor, names, {})
    else
        io.write(tostring(t) .. "\n")
    end
end

function Rhodium.internal.copyTable(t)
    if type(t) == "table" then
        local t2 = {}
        for i, v in pairs(t) do
            if type(v) == "table" then
                t2[i] = Rhodium.internal.copyTable(v)
            else
                t2[i] = v
            end
        end
        return t2
    else
        return t
    end
end

function Rhodium.math.sign(number)
    return (number > 0 and 1 or (number == 0 and 0 or -1))
end

--[[
    // compute normal
    vec3 capNormal( in vec3 pos, in vec3 a, in vec3 b, in float r )
    {
        vec3  ba = b - a;
        vec3  pa = pos - a;
        float h = clamp(dot(pa,ba)/dot(ba,ba),0.0,1.0);
        return (pa - h*ba)/r;
    }
]]


--- returns the normal of a capsule at a point
---@param x number
---@param y number
---@param z number
---@param aX number
---@param aY number
---@param aZ number
---@param bX number
---@param bY number
---@param bZ number
---@return number
---@return number
---@return number
function Rhodium.math.capsuleNormal(x, y, z, aX, aY, aZ, bX, bY, bZ)
    local baX, baY, baZ = bX - aX, bY - aY, bZ - aZ
    local paX, paY, paZ = x - aX, y - aY, z - aZ
    local h = Rhodium.math.clamp(
        Rhodium.math.dot(baX, baY, baZ, paX, paY, paZ) / Rhodium.math.dot(baX, baY, baZ, baX, baY, baZ), 0.0,
        1.0)
    return Rhodium.math.normalize3(paX - h * baX, paY - h * baY, paZ - h * baZ)
end

function Rhodium.math.calculateSphereTangent(normal)
    -- Check if the normal is parallel to the up vector
    local up = vec3(0, 1, 0)
    local tangent = normal:cross(up)
    if tangent:length() < 1e-6 then
        up:release()
        tangent:release()
        return vec3(1, 0, 0)
    else
        up:release()
        local t = tangent:normalize()
        tangent:release()
        return t
    end
end

function Rhodium.math.uvSphere(radius, segments)
    local t = {}
    -- precalculate pi * segments and pi / segments
    local piInverseSegments = math.pi / segments

    -- cos(0) = 1, sin(0) = 0
    local cosR, sinR = 1, 0
    -- redefine the stack variables to be local, this is faster
    -- inverse pi, inverse pi * 2, pi * 0.5
    for r = 0, PI2 - piInverseSegments, piInverseSegments do
        local r2 = r + piInverseSegments
        --math.cos(-PI05) * radius = -1 * radius = -radius, math.sin(PI05) * radius = 0 * radius = 0
        local cosARadius = 0
        local sinARadius = -radius
        -- calculate cos(r) and sin(r) for the next iteration
        local cosR2 = math.cos(r2)
        local sinR2 = math.sin(r2)
        for a = -PI05, PI05, piInverseSegments do
            local a2 = a + piInverseSegments

            local cosA2Radius = math.cos(a2) * radius
            local sinA2Radius = math.sin(a2) * radius

            table.insert(t, {
                cosR * cosARadius,
                sinARadius,
                sinR * cosARadius,
                r * IPI2,
                (a + PI05) * IPI
            })
            table.insert(t, {
                cosR2 * cosARadius,
                sinARadius,
                sinR2 * cosARadius,
                r * IPI2,
                (a2 + PI05) * IPI
            })
            table.insert(t, {
                cosR * cosA2Radius,
                sinA2Radius,
                sinR * cosA2Radius,
                r2 * IPI2,
                (a + PI05) * IPI
            })

            table.insert(t, {
                cosR * cosA2Radius,
                sinA2Radius,
                sinR * cosA2Radius,
                r * IPI2,
                (a + PI05) * IPI
            })
            table.insert(t, {
                cosR2 * cosA2Radius,
                sinA2Radius,
                sinR2 * cosA2Radius,
                r * IPI2,
                (a2 + PI05) * IPI
            })
            table.insert(t, {
                cosR2 * cosARadius,
                sinARadius,
                sinR2 * cosARadius,
                r2 * IPI2,
                (a + PI05) * IPI
            })
            -- since cosA2 = cosA + piInverseSegments and we loop to the next iteration we can just set cosA to cosA2
            cosARadius = cosA2Radius
            sinARadius = sinA2Radius
        end
        cosR = cosR2
        sinR = sinR2
    end
    for i, vertex in ipairs(t) do
        vertex[6], vertex[7], vertex[8] = Rhodium.math.normalize3(vertex[1], vertex[2], vertex[3])
    end
    return t
end

function Rhodium.internal.newID(i)
    i = i or "global"
    Rhodium.internal.idCounters[i] = (Rhodium.internal.idCounters[i] or 0) + 1
    return Rhodium.internal.idCounters[i] - 1
end

function Rhodium.math.mix(i, v, w)
    if type(v) == "table" then
        if type(w) == "table" then
            if type(i) == "table" then
                local t = {}
                for j = 1, #v do
                    t[j] = (1 - i[j]) * v[j] + i[j] * w[j]
                end
                return t
            else
                local t = {}
                for j = 1, #v do
                    t[j] = (1 - i) * v[j] + i * w[j]
                end
                return t
            end
        else
            if type(i) == "table" then
                local t = {}
                for j = 1, #v do
                    t[j] = (1 - i[j]) * v[j] + i[j] * w
                end
                return t
            else
                local t = {}
                for j = 1, #v do
                    t[j] = (1 - i) * v[j] + i * w
                end
                return t
            end
        end
    else
        if type(w) == "table" then
            if type(i) == "table" then
                local t = {}
                for j = 1, #w do
                    t[j] = (1 - i[j]) * v + i[j] * w[j]
                end
                return t
            else
                local t = {}
                for j = 1, #w do
                    t[j] = (1 - i) * v + i * w[j]
                end
                return t
            end
        else
            return (1 - i) * v + i * w
        end
    end
end

function Rhodium.math.cross(x1, y1, z1, x2, y2, z2)
    return y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2
end

function Rhodium.math.dot(x1, y1, z1, x2, y2, z2)
    return x1 * x2 + y1 * y2 + z1 * z2
end

function Rhodium.math.dot2(x1, y1, x2, y2)
    return x1 * x2 + y1 * y2
end

function Rhodium.math.dot3(x1, y1, z1, x2, y2, z2)
    return x1 * x2 + y1 * y2 + z1 * z2
end

function Rhodium.math.dot4(x1, y1, z1, w1, x2, y2, z2, w2)
    return x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
end

function Rhodium.math.lerp(angle, target, turnrate)
    local dist = target - angle
    dist = (dist + math.pi) % PI2 - math.pi
    local step = turnrate * love.timer.getDelta()
    if math.abs(dist) <= step then
        angle = target
    else
        if dist < 0 then
            step = -step
        end
        angle = angle + step
    end
    return angle
end

function Rhodium.math.smoothLerp(angle, target, turnrate)
    local dist = target - angle
    dist = (dist + math.pi) % (math.pi * 2) - math.pi
    local step = turnrate * love.timer.getDelta()
    return angle + step * dist
end

function Rhodium.math.pointAABBDistance(min, max, position)
    local d = 0
    if position.x < min.x then
        d = d + (position.x - min.x) ^ 2
    elseif position.x > max.x then
        d = d + (position.x - max.x) ^ 2
    end
    if position.y < min.y then
        d = d + (position.y - min.y) ^ 2
    elseif position.y > max.y then
        d = d + (position.y - max.y) ^ 2
    end
    if position.z < min.z then
        d = d + (position.z - min.z) ^ 2
    elseif position.z > max.z then
        d = d + (position.z - max.z) ^ 2
    end
    return math.sqrt(d)
end

function Rhodium.math.pointAABBDistanceSqr(min, max, position)
    local d = 0
    if position.x < min.x then
        d = d + (position.x - min.x) ^ 2
    elseif position.x > max.x then
        d = d + (position.x - max.x) ^ 2
    end
    if position.y < min.y then
        d = d + (position.y - min.y) ^ 2
    elseif position.y > max.y then
        d = d + (position.y - max.y) ^ 2
    end
    if position.z < min.z then
        d = d + (position.z - min.z) ^ 2
    elseif position.z > max.z then
        d = d + (position.z - max.z) ^ 2
    end
    return d
end

--- switch between functions, select with i
---@param i number
---@param ... function
function Rhodium.math.switch(i, ...)
    local t = { ... }
    t[i]()
end

function Rhodium.math.point_line_distance(p, v1, v2)
    local AB = v2 - v1
    return (AB:cross(p - v1)):length() / AB:length()
end

do -- define rotation conversions
    -- Other to matrix:

    ---@return matrix4x4 m
    function Rhodium.math.eulerToMatrix(pitch, yaw, roll)
        -- this function assumes pitch is about the z-axis rather than the x-axis (??)
        -- so i swapped pitch and roll
        local ch = math.cos(yaw)
        local sh = math.sin(yaw)
        local ca = math.cos(roll)
        local sa = math.sin(roll)
        local cb = math.cos(pitch)
        local sb = math.sin(pitch)

        local m = mat4()
        m[1][1] = ch * ca
        m[1][2] = sh * sb - ch * sa * cb
        m[1][3] = ch * sa * sb + sh * cb
        m[2][1] = sa
        m[2][2] = ca * cb
        m[2][3] = -ca * sb
        m[3][1] = -sh * ca
        m[3][2] = sh * sa * cb + ch * sb
        m[3][3] = -sh * sa * sb + ch * cb

        return m
    end

    ---@return matrix4x4 m
    function Rhodium.math.quaternionToMatrix(q)
        local sqw = q.w * q.w
        local sqx = q.x * q.x
        local sqy = q.y * q.y
        local sqz = q.z * q.z

        -- invs (inverse square length) is only required if quaternion is not already normalised
        local invs = 1 / (sqx + sqy + sqz + sqw)
        local m = mat4()
        m[1][1] = (sqx - sqy - sqz + sqw) * invs
        m[2][2] = (-sqx + sqy - sqz + sqw) * invs
        m[3][3] = (-sqx - sqy + sqz + sqw) * invs

        local tmp1 = q.x * q.y
        local tmp2 = q.z * q.w
        m[2][1] = 2.0 * (tmp1 + tmp2) * invs
        m[1][2] = 2.0 * (tmp1 - tmp2) * invs

        tmp1 = q.x * q.z
        tmp2 = q.y * q.w
        m[3][1] = 2.0 * (tmp1 - tmp2) * invs
        m[1][3] = 2.0 * (tmp1 + tmp2) * invs
        tmp1 = q.y * q.z
        tmp2 = q.x * q.w
        m[3][2] = 2.0 * (tmp1 + tmp2) * invs
        m[2][3] = 2.0 * (tmp1 - tmp2) * invs

        return m
    end

    -- Other to Quaternion:

    ---@return quaternion quat
    function Rhodium.math.eulerToQuaternion(pitch, yaw, roll)
        if type(pitch) == "table" or not pitch or not yaw or not roll then
            error("Rhodium.math.eulerToQuaternion: invalid input")
        end
        pitch = pitch * 0.5
        yaw = yaw * 0.5
        roll = roll * 0.5

        local c1 = math.cos(yaw)
        local s1 = math.sin(yaw)

        local c2 = math.cos(roll)
        local s2 = math.sin(roll)

        local c3 = math.cos(pitch)
        local s3 = math.sin(pitch)

        local c1c2 = c1 * c2
        local s1s2 = s1 * s2
        local w = c1c2 * c3 - s1s2 * s3
        local x = c1c2 * s3 + s1s2 * c3
        local y = s1 * c2 * c3 + c1 * s2 * s3
        local z = c1 * s2 * c3 - s1 * c2 * s3
        return quaternion(x, y, z, w)
    end

    ---@return quaternion quat
    function Rhodium.math.matrixToQuaternion(m)
        local a = m:transpose()
        local trace = a[1][1] + a[2][2] + a[3][3]
        local q = quaternion()

        if trace > 0 then
            local s = 0.5 / math.sqrt(trace + 1.0)
            q.w = 0.25 / s
            q.x = (a[3][2] - a[2][3]) * s
            q.y = (a[1][3] - a[3][1]) * s
            q.z = (a[2][1] - a[1][2]) * s
        elseif a[1][1] > a[2][2] and a[1][1] > a[3][3] then
            local s = 2.0 * math.sqrt(1.0 + a[1][1] - a[2][2] - a[3][3])
            q.w = (a[3][2] - a[2][3]) / s
            q.x = 0.25 * s
            q.y = (a[1][2] + a[2][1]) / s
            q.z = (a[1][3] + a[3][1]) / s
        elseif a[2][2] > a[3][3] then
            local s = 2.0 * math.sqrt(1.0 + a[2][2] - a[1][1] - a[3][3])
            q.w = (a[1][3] - a[3][1]) / s
            q.x = (a[1][2] + a[2][1]) / s
            q.y = 0.25 * s
            q.z = (a[2][3] + a[3][2]) / s
        else
            local s = 2.0 * math.sqrt(1.0 + a[3][3] - a[1][1] - a[2][2])
            q.w = (a[2][1] - a[1][2]) / s
            q.x = (a[1][3] + a[3][1]) / s
            q.y = (a[2][3] + a[3][2]) / s
            q.z = 0.25 * s
        end

        return q
    end

    -- Other to euler:

    ---@return number pitch
    ---@return number yaw
    ---@return number roll
    function Rhodium.math.quaternionToEuler(q)
        local test = q.x * q.y + q.z * q.w
        local heading, attitude, bank

        if test > 0.499 then -- singularity at north pole
            heading = 2 * math.atan2(q.x, q.w)
            attitude = math.pi / 2
            bank = 0
        elseif test < -0.499 then -- singularity at south pole
            heading = -2 * math.atan2(q.x, q.w)
            attitude = -math.pi / 2
            bank = 0
        else
            local sqx = q.x * q.x
            local sqy = q.y * q.y
            local sqz = q.z * q.z
            heading = math.atan2(2 * q.y * q.w - 2 * q.x * q.z, 1 - 2 * sqy - 2 * sqz)
            attitude = math.asin(2 * test)
            bank = math.atan2(2 * q.x * q.w - 2 * q.y * q.z, 1 - 2 * sqx - 2 * sqz)
        end
        -- this function assumes pitch is about the z-axis rather than the x-axis (??)
        return bank, heading, attitude
    end

    ---@return number pitch
    ---@return number yaw
    ---@return number roll
    function Rhodium.math.matrixToEuler(m)
        -- Assuming the angles are in radians.
        local heading, attitude, bank
        if m[2][1] > 0.998 then -- singularity at north pole
            heading = math.atan2(m[1][3], m[3][3])
            attitude = math.pi / 2
            bank = 0
        elseif m[2][1] < -0.998 then -- singularity at south pole
            heading = math.atan2(m[1][3], m[3][3])
            attitude = -math.pi / 2
            bank = 0
        else
            heading = math.atan2(-m[3][1], m[1][1])
            bank = math.atan2(-m[2][3], m[2][2])
            attitude = math.asin(m[2][1])
        end

        -- this function assumes pitch is about the z-axis rather than the x-axis (??)
        return bank, heading, attitude
    end
end

--- creates a new translation matrix
---@param position vec3
---@return matrix4x4
function Rhodium.math.newTranslationMatrix(position)
    local m = mat4()
    m[4][1] = position.x
    m[4][2] = position.y
    m[4][3] = position.z
    return m
end

function Rhodium.math.calculateCameraMatrix()
    Camera.rotationMatrix = Rhodium.math.eulerToMatrix(
        -Camera.pitch,
        -Camera.yaw,
        -Camera.roll
    )

    Camera.viewMatrix = Rhodium.math.newTranslationMatrix(-Camera.position) * Camera.rotationMatrix
    Camera.viewProjectionMatrix = Camera.viewMatrix * Camera.projectionMatrix

    return Camera.viewProjectionMatrix
end

---@param matrix matrix4x4
---@return table
function Rhodium.math.frustumFromMatrix(matrix)
    local frustum = {}

    frustum[1] = vec4(
        matrix[1][4] + matrix[1][1],
        matrix[2][4] + matrix[2][1],
        matrix[3][4] + matrix[3][1],
        matrix[4][4] + matrix[4][1]
    )

    frustum[2] = vec4(
        matrix[1][4] - matrix[1][1],
        matrix[2][4] - matrix[2][1],
        matrix[3][4] - matrix[3][1],
        matrix[4][4] - matrix[4][1]
    )

    frustum[3] = vec4(
        matrix[1][4] - matrix[1][2],
        matrix[2][4] - matrix[2][2],
        matrix[3][4] - matrix[3][2],
        matrix[4][4] - matrix[4][2]
    )

    frustum[4] = vec4(
        matrix[1][4] + matrix[1][2],
        matrix[2][4] + matrix[2][2],
        matrix[3][4] + matrix[3][2],
        matrix[4][4] + matrix[4][2]
    )

    frustum[5] = vec4(
        matrix[1][4] + matrix[1][3],
        matrix[2][4] + matrix[2][3],
        matrix[3][4] + matrix[3][3],
        matrix[4][4] + matrix[4][3]
    )

    frustum[6] = vec4(
        matrix[1][4] - matrix[1][3],
        matrix[2][4] - matrix[2][3],
        matrix[3][4] - matrix[3][3],
        matrix[4][4] - matrix[4][3]
    )

    return {
        frustum = frustum,
    }
end

function Rhodium.math.frustumAABB(fru, box)
    local x, y, z, w, h, d = unpack(box)
    local x1, y1, z1 = x + w, y + h, z + d
    local dot = Rhodium.math.dot
    for i = 1, 6 do
        local frustum = fru.frustum[i]
        local fx, fy, fz, fw = frustum:get()
        if dot(fx, fy, fz, x, y, z) + fw < 0.0
            and dot(fx, fy, fz, x1, y, z) + fw < 0.0
            and dot(fx, fy, fz, x, y1, z) + fw < 0.0
            and dot(fx, fy, fz, x1, y1, z) + fw < 0.0
            and dot(fx, fy, fz, x, y, z1) + fw < 0.0
            and dot(fx, fy, fz, x1, y, z1) + fw < 0.0
            and dot(fx, fy, fz, x, y1, z1) + fw < 0.0
            and dot(fx, fy, fz, x1, y1, z1) + fw < 0.0 then
            return false
        end
    end

    --local out
    --out=0; for i = 1,8 do out = out + ((fru.corners[i][1] > x1) and 1 or 0) end; if out == 8 then return false end
    --out=0; for i = 1,8 do out = out + ((fru.corners[i][1] < x) and 1 or 0) end; if out == 8 then return false end
    --out=0; for i = 1,8 do out = out + ((fru.corners[i][2] > y1) and 1 or 0) end; if out == 8 then return false end
    --out=0; for i = 1,8 do out = out + ((fru.corners[i][2] < y) and 1 or 0) end; if out == 8 then return false end
    --out=0; for i = 1,8 do out = out + ((fru.corners[i][3] > z1) and 1 or 0) end; if out == 8 then return false end
    --out=0; for i = 1,8 do out = out + ((fru.corners[i][3] < z) and 1 or 0) end; if out == 8 then return false end

    return true
end

---returns a ray that goes from the camera position to the x,y position
---@param x number
---@param y number
---@return ray ray length undefined
function Rhodium.math.screenPositionToRay(x, y)
    local clip = vec4(
        (x / love.graphics.getWidth() - 0.5) * 2,
        -(y / love.graphics.getHeight() - 0.5) * 2,
        1, 1
    )

    local view = Rhodium.internal.graphicsData.cameraProjectionMatrix:transpose():invert():vMul(clip)
    view = view / view.w
    local world = Rhodium.internal.graphicsData.viewMatrix:transpose():invert():vMul(view)
    local cameraPosition = Rhodium.internal.graphicsData.cameraPosition
    local worldPos = vec3(world)
    local data = { position = cameraPosition, direction = (worldPos - cameraPosition):normalize() }
    worldPos:release()
    return data
end

---@deprecated
function Rhodium.math.eulerFromMatrix(m)
    error("Rhodium.math.eulerFromMatrix: renamed to Rhodium.math.matrixToEuler")
end

function Rhodium.math.length(...)
    local val = { ... }
    if type(val[1]) == "table" then
        if val[1][1] == nil then
            local x, y, z, w = val[1].x or 0, val[1].y or 0, val[1].z or 0, val[1].w or 0
            return math.sqrt(x * x + y * y + z * z + w * w)
        else
            local v = 0
            for _, w in ipairs(val[1]) do v = v + w * w end
            return math.sqrt(v)
        end
    else
        local v = 0
        for _, w in ipairs(val) do v = v + w * w end
        return math.sqrt(v)
    end
end

function Rhodium.math.length2(x, y)
    return math.sqrt(x * x + y * y)
end

function Rhodium.math.length3(x, y, z)
    return math.sqrt(x * x + y * y + z * z)
end

function Rhodium.math.length4(x, y, z, w)
    return math.sqrt(x * x + y * y + z * z + w * w)
end

function Rhodium.math.crossVector(v1, v2)
    return vec3(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z, v1.x * v2.y - v1.y * v2.x)
end

function Rhodium.math.dotVector(v1, v2)
    return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
end

--- Rotate a position using a quaternion and vector math
---@param position vec3
---@param quat quaternion
---@return vec3
function Rhodium.math.rotatePosition(position, quat)
    -- position + 2 * q:cross(q:cross(position) + position * quat[4])
    return position + 2 * quat:crossVector(quat:crossVector(position) + position * quat.w)
end

--- Rotate a position using normal variables instead of vectors or quaternions (12x faster than Rhodium.math.rotatePosition)
---@param x number -- position x
---@param y number -- position y
---@param z number -- position z
---@param qx number -- quaternion x
---@param qy number -- quaternion y
---@param qz number -- quaternion z
---@param qw number -- quaternion w
---@return number, number, number
function Rhodium.math.rotatePositionSeperate(x, y, z, qx, qy, qz, qw)
    local cx = qy * z - qz * y + x * qw
    local cy = qz * x - qx * z + y * qw
    local cz = qx * y - qy * x + z * qw

    return x + 2 * (qy * cz - qz * cy),
        y + 2 * (qz * cx - qx * cz),
        z + 2 * (qx * cy - qy * cx)
end

--- converts vertices and indices to a triangle list
---@param vertices table
---@param indices table<integer>
function Rhodium.math.verticesToTriangles(vertices, indices, triangles)
    if #indices % 3 ~= 0 then
        error("Rhodium.math.verticesToTriangles: invalid indices")
    end
    triangles = triangles or {}
    for i = 1, #indices do
        table.insert(triangles, vertices[indices[i]])
    end
    return triangles
end

function Rhodium.math.rotatePositions(...)
    local t = { ... }
    local q = t[#t]
    local vertices = {}
    for i = 1, #t - 1 do
        table.insert(vertices, Rhodium.math.rotatePosition(t[i], q))
    end
    return vertices
end

--- Rotate positions using normal variables instead of vectors or quaternions (12x faster than Rhodium.math.rotatePosition)
function Rhodium.math.rotatePositionsSeperate(...)
    local t = { ... }
    local qx, qy, qz, qw = t[#t][1], t[#t][2], t[#t][3], t[#t][4]
    local vertices = {}
    for i = 1, #t - 1 do
        local x, y, z = t[i][1], t[i][2], t[i][3]
        local cx = qy * z - qz * y + x * qw
        local cy = qz * x - qx * z + y * qw
        local cz = qx * y - qy * x + z * qw

        table.insert(vertices, {
            x + 2 * (qy * cz - qz * cy),
            y + 2 * (qz * cx - qx * cz),
            z + 2 * (qx * cy - qy * cx)
        })
    end
    return vertices
end

--- Rotate positions using normal variables instead of vectors or quaternions (12x faster than Rhodium.math.rotatePosition)
function Rhodium.math.rotateTablePositionsSeperate(vertices, qx, qy, qz, qw)
    local newPoints = {}
    for i = 1, #vertices do
        local x, y, z = vertices[i][1], vertices[i][2], vertices[i][3]
        local cx = qy * z - qz * y + x * qw
        local cy = qz * x - qx * z + y * qw
        local cz = qx * y - qy * x + z * qw

        table.insert(newPoints, {
            x + 2 * (qy * cz - qz * cy),
            y + 2 * (qz * cx - qx * cz),
            z + 2 * (qx * cy - qy * cx)
        })
    end
    return newPoints
end

--- calculates the triangle normal of a triangle
---@param p1 table point 1
---@param p2 table point 2
---@param p3 table point 3
---@param inverted? boolean invert the normal?
---@return number x
---@return number y
---@return number z
function Rhodium.math.triangleNormal(p1, p2, p3, inverted)
    local ux, uy, uz = p2[1] - p1[1], p2[2] - p1[2], p2[3] - p1[3]
    local vx, vy, vz = p3[1] - p1[1], p3[2] - p1[2], p3[3] - p1[3]
    local x = (uy * vz - uz * vy) * (inverted and -1 or 1)
    local y = (uz * vx - ux * vz) * (inverted and -1 or 1)
    local z = (ux * vy - uy * vx) * (inverted and -1 or 1)
    return Rhodium.math.normalize3(x, y, z)
end

function Rhodium.math.normalize(...)
    local t = { ... }
    if type(t[1]) == "table" then
        t = t[1]
        local d = 0
        for i = 1, #t do
            d = d + t[i] * t[i]
        end
        local d1 = 1 / math.sqrt(d)
        local t2 = {}
        for i = 1, #t do
            if d == 0 then
                t2[i] = 0
            else
                t2[i] = t[i] * d1
            end
        end
        return unpack(t2)
    else
        local d = 0
        for i = 1, #t do
            d = d + t[i] * t[i]
        end
        local d1 = 1 / math.sqrt(d)
        local t2 = {}
        for i = 1, #t do
            if d == 0 then
                t2[i] = 0
            else
                t2[i] = t[i] * d1
            end
        end
        return unpack(t2)
    end
end

function Rhodium.math.normalize2(x, y)
    local d = x * x + y * y
    if d == 0 then
        return 0.0, 0.0
    end
    local d1 = 1 / math.sqrt(d)
    return x * d1, y * d1
end

function Rhodium.math.normalize3(x, y, z)
    local d = x * x + y * y + z * z
    if d == 0 then
        return 0.0, 0.0, 0.0
    end
    d = 1 / math.sqrt(d)
    return x * d, y * d, z * d
end

function Rhodium.math.normalize4(x, y, z, w)
    local d = x * x + y * y + z * z + w * w
    if d == 0 then
        return 0, 0, 0, 0
    end
    local d1 = 1 / math.sqrt(d)
    return x * d1, y * d1, z * d1, w * d1
end

function Rhodium.math.closestPointOnTriangle(a, b, c, point)
    local px, py, pz = point[1], point[2], point[3]

    local abX = b[1] - a[1]
    local abY = b[2] - a[2]
    local abZ = b[3] - a[3]

    local acX = c[1] - a[1]
    local acY = c[2] - a[2]
    local acZ = c[3] - a[3]

    local apX = px - a[1]
    local apY = py - a[2]
    local apZ = pz - a[3]

    local d1 = Rhodium.math.dot(abX, abY, abZ, apX, apY, apZ)
    local d2 = Rhodium.math.dot(acX, acY, acZ, apX, apY, apZ)
    if d1 <= 0 and d2 <= 0 then
        return vec3(a)
    end

    local bpX = px - b[1]
    local bpY = py - b[2]
    local bpZ = pz - b[3]

    local d3 = Rhodium.math.dot(abX, abY, abZ, bpX, bpY, bpZ)
    local d4 = Rhodium.math.dot(acX, acY, acZ, bpX, bpY, bpZ)
    if d3 >= 0 and d4 <= d3 then
        return vec3(b)
    end

    local vc = d1 * d4 - d3 * d2
    if vc <= 0 and d1 >= 0 and d3 <= 0 then
        local v = d1 / (d1 - d3)
        local temp = vec3(abX, abY, abZ) * v
        local point = vec3(a) + temp
        temp:release()
        return point
    end

    local cpX = px - c[1]
    local cpY = py - c[2]
    local cpZ = pz - c[3]

    local d5 = Rhodium.math.dot(abX, abY, abZ, cpX, cpY, cpZ)
    local d6 = Rhodium.math.dot(acX, acY, acZ, cpX, cpY, cpZ)
    if d6 >= 0 and d5 <= d6 then
        return vec3(c)
    end

    local vb = d5 * d2 - d1 * d6
    if vb <= 0 and d2 >= 0 and d6 <= 0 then
        local w = d2 / (d2 - d6)
        local temp = vec3(acX, acY, acZ) * w
        local point = vec3(a) + temp
        temp:release()
        return point
    end

    local va = d3 * d6 - d5 * d4
    if va <= 0 and (d4 - d3) >= 0 and (d5 - d6) >= 0 then
        local w = (d4 - d3) / ((d4 - d3) + (d5 - d6))
        local vec3C = vec3(c)
        local vec3B = vec3(b)
        local temp = vec3C - vec3B
        local point = vec3B + temp * w
        temp:release()
        vec3C:release()
        vec3B:release()
        return point
    end

    local denom = 1 / (va + vb + vc)
    local v = vb * denom
    local w = vc * denom
    local vec3A = vec3(a)
    local vec3AB = vec3(abX, abY, abZ)
    local vec3AC = vec3(acX, acY, acZ)
    local point = vec3A + vec3AB * v + vec3AC * w
    vec3A:release()
    vec3AB:release()
    vec3AC:release()
    return point
end

--- closest point between two triangles
---@param a table<number, number, number>
---@param b table<number, number, number>
---@param c table<number, number, number>
---@param d table<number, number, number>
---@param e table<number, number, number>
---@param f table<number, number, number>
---@return vec3
function Rhodium.math.closestPointToTriangles(a, b, c, d, e, f)
    local closestPointFromTriA
    local closestDistanceToTriB = math.huge
    for index, vert in ipairs({ a, b, c }) do
        local point = Rhodium.math.closestPointOnTriangle(d, e, f, vert)
        local dist = (point - vert):lengthSqr()
        if dist < closestDistanceToTriB then
            closestDistanceToTriB = dist
            closestPointFromTriA = point -- or point
        end
    end
    local closestPointOnTriA = Rhodium.math.closestPointOnTriangle(a, b, c, closestPointFromTriA)

    local closestPointFromTriB
    local closestDistanceToTriA = math.huge
    for index, vert in ipairs({ d, e, f }) do
        local point = Rhodium.math.closestPointOnTriangle(a, b, c, vert)
        local dist = (point - vert):lengthSqr()
        if dist < closestDistanceToTriA then
            closestDistanceToTriA = dist
            closestPointFromTriB = point -- or point
        end
    end
    local closestPointOnTriB = Rhodium.math.closestPointOnTriangle(d, e, f, closestPointFromTriB)

    -- find the triangle that is closest to the avarage contact position
    local averageContactPosition = (closestPointOnTriA + closestPointOnTriB) * 0.5
    local closestTriangleIndex = nil
    local closestDistance = math.huge
    for index, triangle in ipairs({ { a, b, c }, { d, e, f } }) do
        local distance = math.huge
        for vertIndex, vert in ipairs(triangle) do
            local dist = (vert - averageContactPosition):lengthSqr()
            if dist < distance then
                distance = dist
            end
        end
        if distance < closestDistance then
            closestDistance = distance
            closestTriangleIndex = index
        end
    end
    return closestTriangleIndex == 1 and closestPointOnTriA or closestPointOnTriB
end

function Rhodium.math.eulerToAxisAngle(pitch, yaw, roll)
    local c1 = math.cos(yaw / 2)
    local s1 = math.sin(yaw / 2)
    local c2 = math.cos(pitch / 2)
    local s2 = math.sin(pitch / 2)
    local c3 = math.cos(roll / 2)
    local s3 = math.sin(roll / 2)
    local c1c2 = c1 * c2
    local s1s2 = s1 * s2
    local w = c1c2 * c3 - s1s2 * s3
    local x = c1c2 * s3 + s1s2 * c3
    local y = s1 * c2 * c3 + c1 * s2 * s3
    local z = c1 * s2 * c3 - s1 * c2 * s3
    local angle = 2 * math.acos(w)
    local norm = 1 / (x * x + y * y + z * z)
    if norm < 0.001 then
        x = 1
        y, z = 0, 0
    else
        norm = math.sqrt(norm);
        x = x * norm
        y = y * norm
        z = z * norm
    end
    return vec4(x, y, z, angle)
end

function Rhodium.math.axisAngleToEuler(axisAngle)
    local x, y, z, angle = axisAngle:get()
    local s = math.sin(angle)
    local c = math.cos(angle)
    local t = 1 - c
    if (x * y * t + z * s) > 0.998 then
        yaw = 2 * math.atan2(x * math.sin(angle / 2), math.cos(angle / 2))
        pitch = PI05
        roll = 0
        return pitch, yaw, roll
    end
    if (x * y * t + z * s) < -0.998 then
        yaw = -2 * math.atan2(x * math.sin(angle / 2), math.cos(angle / 2))
        pitch = -PI05
        roll = 0
        return pitch, yaw, roll
    end
    yaw = math.atan2(y * s - x * z * t, 1 - (y * y + z * z) * t)
    pitch = math.asin(x * y * t + z * s)
    roll = math.atan2(x * s - y * z * t, 1 - (x * x + z * z) * t)
    return pitch, yaw, roll
end

function Rhodium.math.axisAngleToQuat(axisAngle)
    local angle = axisAngle[4] * 0.5
    local sinAngle = math.sin(angle)
    return quaternion(
        axisAngle[1] * sinAngle,
        axisAngle[2] * sinAngle,
        axisAngle[3] * sinAngle,
        math.cos(angle)
    )
end

function Rhodium.math.quatToAxisAngle(quat)
    local angle = math.acos(quat[4]) * 2
    local mul = 1 / math.sqrt(1 - quat[4] * quat[4])
    if mul > 1000 then
        return vec4(
            quat[1],
            quat[2],
            quat[3],
            math.cos(angle)
        )
    else
        return vec4(
            quat[1] * mul,
            quat[2] * mul,
            quat[3] * mul,
            math.cos(angle)
        )
    end
end

--- combines strings and numbers into a string
---@param ... any strings and numbers, last argument can be a table with settings {separator = " "}
function Rhodium.internal.combine(...)
    local input = { ... }
    local settings = input[#input]
    if type(settings) == "table" then
        table.remove(input, #input)
    else
        settings = {
            seperator = " ",
        }
    end
    local output = ""

    for i = 1, #input do
        if type(input[i]) ~= "string" then
            output = output .. tostring(input[i]) .. (i ~= #input and settings.seperator or "")
        else
            output = output .. input[i] .. (i ~= #input and settings.seperator or "")
        end
    end
end

--- returns the type of a variable
---@param x any
---@return "vec2"|"vec3"|"vec4"|"quaternion"|"physicsBody"|"physicsShape"|"unknown"|"spotLight"|"sunLight"|"pointLight"|"areaLight"|"volume"|"uiConstraintPoint"
function Rhodium.math.type(x)
    local t = type(x)
    if t == "cdata" then
        local ct = ffi.typeof(x)

        if ct == Rhodium.internal.types.vec2 then
            return "vec2"
        elseif ct == Rhodium.internal.types.vec3 then
            return "vec3"
        elseif ct == Rhodium.internal.types.vec4 then
            return "vec4"
        elseif ct == Rhodium.internal.types.quaternion then
            return "quaternion"
        else
            return "unknown"
        end
    elseif t == "table" then
        -- check for Rhodium.jolt.body, Rhodium.physicsShape
        if x.type == "physicsBody" then
            return "physicsBody"
        elseif x.type == "physicsShape" then
            return "physicsShape"
        elseif x.type == "spot" then
            return "spotLight"
        elseif x.type == "sun" then
            return "sunLight"
        elseif x.type == "point" then
            return "pointLight"
        elseif x.type == "area" then
            return "areaLight"
        elseif x.type == "volume" then
            return "volume"
        elseif x.type == "uiConstraintPoint" then
            return "uiConstraintPoint"
        else
            return "unknown"
        end
    else
        return "unknown"
    end
end

function Rhodium.math.rayTorus(r, tor, position, quat)
    local ray = {}
    do -- reposition the ray to account for the fact that i can't rotate the torus
        ray.position = Rhodium.math.rotatePosition(r.position - position, quat)
        ray.direction = Rhodium.math.rotatePosition(r.direction, quat)
    end
    local po = 1.0

    local Ra2 = tor.x * tor.x
    local ra2 = tor.y * tor.y

    local m = Rhodium.math.dotVector(ray.position, ray.position)
    local n = Rhodium.math.dotVector(ray.position, ray.direction)

    local k = (m - ra2 - Ra2) / 2.0
    local k3 = n
    local k2 = n * n + Ra2 * ray.direction.z * ray.direction.z + k
    local k1 = k * n + Ra2 * ray.position.z * ray.direction.z
    local k0 = k * k + Ra2 * ray.position.z * ray.position.z - Ra2 * ra2

    if math.abs(k3 * (k3 * k3 - k2) + k1) < 0.01 then
        po = -1.0
        local tmp = k1
        k1 = k3
        k3 = tmp
        k0 = 1.0 / k0
        k1 = k1 * k0
        k2 = k2 * k0
        k3 = k3 * k0
    end

    local c2 = 2.0 * k2 - 3.0 * k3 * k3
    local c1 = k3 * (k3 * k3 - k2) + k1
    local c0 = k3 * (k3 * (c2 + 2.0 * k2) - 8.0 * k1) + 4.0 * k0


    c2 = c2 / 3.0
    c1 = c1 * 2.0
    c0 = c0 / 3.0

    local Q = c2 * c2 + c0
    local R = c2 * c2 * c2 - 3.0 * c2 * c0 + c1 * c1

    local h = R * R - Q * Q * Q

    if h >= 0.0 then
        h = math.sqrt(h)
        local v = Rhodium.math.sign(R + h) * (math.abs(R + h) ^ (1.0 / 3.0))
        local u = Rhodium.math.sign(R - h) * (math.abs(R - h) ^ (1.0 / 3.0))
        s = vec3((v + u) + 4.0 * c2, (v - u) * math.sqrt(3.0))
        local y = math.sqrt(0.5 * (s:length() + s[1]))
        local x = 0.5 * s.y / y
        local r = 2.0 * c1 / (x * x + y * y)
        local t1 = x - r - k3
        t1 = (po < 0.0) and 2.0 / t1 or t1
        local t2 = -x - r - k3
        t2 = (po < 0.0) and 2.0 / t2 or t2
        local t = math.huge
        if t1 > 0.0 then t = t1 end
        if t2 > 0.0 then t = math.min(t, t2) end
        return t > 0 and t or nil
    end

    local sQ = math.sqrt(Q)
    local w = sQ * math.cos(math.acos(-R / (sQ * Q)) / 3.0)
    local d2 = -(w + c2)
    if d2 < 0.0 then return nil end
    local d1 = math.sqrt(d2)
    local h1 = math.sqrt(w - 2.0 * c2 + c1 / d1)
    local h2 = math.sqrt(w - 2.0 * c2 - c1 / d1)
    local t1 = -d1 - h1 - k3
    t1 = (po < 0.0) and 2.0 / t1 or t1
    local t2 = -d1 + h1 - k3
    t2 = (po < 0.0) and 2.0 / t2 or t2
    local t3 = d1 - h2 - k3
    t3 = (po < 0.0) and 2.0 / t3 or t3
    local t4 = d1 + h2 - k3
    t4 = (po < 0.0) and 2.0 / t4 or t4
    local t = math.huge
    if t1 > 0.0 then t = t1 end
    if t2 > 0.0 then t = math.min(t, t2) end
    if t3 > 0.0 then t = math.min(t, t3) end
    if t4 > 0.0 then t = math.min(t, t4) end
    return t > 0 and t or nil
end

function Rhodium.math.rayCapsule(ray, topX, topY, topZ, baseX, baseY, baseZ, radius)
    local dot = Rhodium.math.dot
    local baX, baY, baZ = baseX - topX, baseY - topY, baseZ - topZ
    local oaX, oaY, oaZ = ray.position.x - topX, ray.position.y - topY, ray.position.z - topZ
    local baba = dot(baX, baY, baZ, baX, baY, baZ)
    local bard = dot(baX, baY, baZ, ray.direction.x, ray.direction.y, ray.direction.z)
    local baoa = dot(baX, baY, baZ, oaX, oaY, oaZ)
    local rdoa = dot(ray.direction.x, ray.direction.y, ray.direction.z, oaX, oaY, oaZ)
    local oaoa = dot(oaX, oaY, oaZ, oaX, oaY, oaZ)
    local a = baba - bard * bard
    local b = baba * rdoa - baoa * bard
    local c = baba * oaoa - baoa * baoa - radius * radius * baba
    local h = b * b - a * c
    if h >= 0.0 then
        local t = (-b - math.sqrt(h)) / a
        local y = baoa + t * bard
        if y > 0.0 and y < baba then
            return t
        end
        local oc = y <= 0.0 and vec3(oaX, oaY, oaZ) or
            vec3(ray.position.x - baseX, ray.position.y - baseY, ray.position.z - baseZ)
        b = dot(ray.direction.x, ray.direction.y, ray.direction.z, oc.x, oc.y, oc.z)
        c = dot(oc.x, oc.y, oc.z, oc.x, oc.y, oc.z) - radius * radius
        h = b * b - c
        if h > 0 then
            return -b - math.sqrt(h)
        end
    end
end

function Rhodium.math.rayCylinder(ray, topX, topY, topZ, baseX, baseY, baseZ, radius)
    local dot = Rhodium.math.dot
    local baX, baY, baZ = baseX - topX, baseY - topY, baseZ - topZ
    local ocX, ocY, ocZ = ray.position.x - topX, ray.position.y - topY, ray.position.z - topZ
    local baba = dot(baX, baY, baZ, baX, baY, baZ)
    local bard = dot(baX, baY, baZ, ray.direction.x, ray.direction.y, ray.direction.z)
    local baoc = dot(baX, baY, baZ, ocX, ocY, ocZ)
    local k2 = baba - bard * bard
    local k1 = baba * dot(ocX, ocY, ocZ, ray.direction.x, ray.direction.y, ray.direction.z) - baoc * bard
    local k0 = baba * dot(ocX, ocY, ocZ, ocX, ocY, ocZ) - baoc * baoc - radius * radius * baba
    local h = k1 * k1 - k2 * k0
    if h < 0 then
        return
    end
    h = math.sqrt(h)
    local t = (-k1 - h) / k2
    local y = baoc + t * bard

    if y > 0 and y < baba then
        return t, (vec3(ocX, ocY, ocZ) + t * ray.direction - vec3(baX, baY, baZ) * y / baba) / radius
    end

    t = (((y < 0) and 0 or baba) - baoc) / bard
    if math.abs(k1 + k2 * t) < h then
        return t, vec3(baX, baY, baZ) * Rhodium.math.sign(y) / math.sqrt(baba)
    end
    return
end

function Rhodium.math.raySphere(ray, x, y, z, radius)
    local ox, oy, oz = ray.position.x - x, ray.position.y - y, ray.position.z - z
    local dx, dy, dz = ray.direction.x, ray.direction.y, ray.direction.z

    local a = dx * dx + dy * dy + dz * dz
    local b = 2 * (dx * ox + dy * oy + dz * oz)
    local c = ox * ox + oy * oy + oz * oz - radius * radius
    local d = b * b - 4 * a * c

    if (d >= 0) then
        local dist = (-b - math.sqrt(d)) / (2 * a)

        if (dist >= 0) then
            local hitPos = ray.position + ray.direction * dist
            local normal = hitPos - vec3(x, y, z)
            return dist, hitPos, normal:normalize()
        end
    end
end

function Rhodium.math.triangleTangent(p1, p2, p3)
    local edge1X, edge1Y, edge1Z = p2[1] - p1[1], p2[2] - p1[2], p2[3] - p1[3]
    local edge2X, edge2Y, edge2Z = p3[1] - p1[1], p3[2] - p1[2], p3[3] - p1[3]

    local deltaUV1X, deltaUV1Y = p2[4] - p1[4], p2[5] - p1[5]
    local deltaUV2X, deltaUV2Y = p3[4] - p1[4], p3[5] - p1[5]

    local f = 1.0 / (deltaUV1X * deltaUV2Y - deltaUV2X * deltaUV1Y)

    local tangentX = f * (deltaUV2Y * edge1X - deltaUV1Y * edge2X)
    local tangentY = f * (deltaUV2Y * edge1Y - deltaUV1Y * edge2Y)
    local tangentZ = f * (deltaUV2Y * edge1Z - deltaUV1Y * edge2Z)

    local i = 1 / math.sqrt(tangentX * tangentX + tangentY * tangentY + tangentZ * tangentZ)

    return tangentX * i, tangentY * i, tangentZ * i
end

function Rhodium.math.newScaleMatrix(scale)
    local mat = mat4()
    mat[1][1] = scale.x
    mat[2][2] = scale.y
    mat[3][3] = scale.z
    return mat
end

function Rhodium.math.slerp(qa, qb, t)
    local qm = quaternion()
    local cosHalfTheta = qa.w * qb.w + qa.x * qb.x + qa.y * qb.y + qa.z * qb.z
    if math.abs(cosHalfTheta) >= 1.0 then
        qm.w = qa.w
        qm.x = qa.x
        qm.y = qa.y
        qm.z = qa.z
        return qm
    end
    local halfTheta = math.acos(cosHalfTheta)
    local sinHalfTheta = math.sqrt(1.0 - cosHalfTheta * cosHalfTheta)
    if math.abs(sinHalfTheta) < 0.001 then
        qm.w = (qa.w * 0.5 + qb.w * 0.5)
        qm.x = (qa.x * 0.5 + qb.x * 0.5)
        qm.y = (qa.y * 0.5 + qb.y * 0.5)
        qm.z = (qa.z * 0.5 + qb.z * 0.5)
        return qm
    end
    local ratioA = math.sin((1 - t) * halfTheta) / sinHalfTheta
    local ratioB = math.sin(t * halfTheta) / sinHalfTheta
    qm.w = (qa.w * ratioA + qb.w * ratioB)
    qm.x = (qa.x * ratioA + qb.x * ratioB)
    qm.y = (qa.y * ratioA + qb.y * ratioB)
    qm.z = (qa.z * ratioA + qb.z * ratioB)
    return qm
end

function Rhodium.math.newTransform(translation, rotation, scale)
    local scaleMatrix = mat4({
        { scale.x, 0,       0,       0 },
        { 0,       scale.y, 0,       0 },
        { 0,       0,       scale.z, 0 },
        { 0,       0,       0,       1 }
    })

    local rotationMatrix = Rhodium.math.quaternionToMatrix(rotation)

    local rotationScaleMatrix = rotationMatrix * scaleMatrix

    rotationScaleMatrix[4][1] = translation.x
    rotationScaleMatrix[4][2] = translation.y
    rotationScaleMatrix[4][3] = translation.z

    return rotationScaleMatrix
end

function Rhodium.math.newGLTFTransform(translation, rotation, scale)
    local rx, ry, rz, rw = rotation:get()

    local scaleMatrix = mat4({
        { scale.x, 0,       0,       0 },
        { 0,       scale.y, 0,       0 },
        { 0,       0,       scale.z, 0 },
        { 0,       0,       0,       1 }
    })

    local rotationMatrix = mat4 {
        1 - 2 * (ry * ry + rz * rz), 2 * (rx * ry - rz * rw), 2 * (rx * rz + ry * rw), 0,
        2 * (rx * ry + rz * rw), 1 - 2 * (rx * rx + rz * rz), 2 * (ry * rz - rx * rw), 0,
        2 * (rx * rz - ry * rw), 2 * (ry * rz + rx * rw), 1 - 2 * (rx * rx + ry * ry), 0,
        0, 0, 0, 1
    }

    -- since this rotation code is column major, we could transpose it but we don't need to
    -- instead of rotationScaleMatrix = rotationMatrix * scaleMatrix we swap the order

    local rotationScaleMatrix = scaleMatrix * rotationMatrix

    rotationScaleMatrix[1][4] = translation.x
    rotationScaleMatrix[2][4] = translation.y
    rotationScaleMatrix[3][4] = translation.z

    return rotationScaleMatrix
end

local gltfToRhodiumQuat

function Rhodium.math.fromGLTFQuaternion(...)
    local quat = quaternion(...)

    --[[
        X- right, Y+ up, Z+ forward
        to
        X+ right, Y+ up, Z- forward
    ]]

    if not gltfToRhodiumQuat then
        gltfToRhodiumQuat = quaternion(0, 0, 1, 0)
    end

    quat = quat * gltfToRhodiumQuat

    return quat
end
