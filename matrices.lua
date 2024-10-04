local mat4Mt = {}
---@class matrix4x4
local mat4F = {
    { 0, 0, 0, 0 },
    { 0, 0, 0, 0 },
    { 0, 0, 0, 0 },
    { 0, 0, 0, 0 }
}
local mat3Mt = {}
---@class matrix3x3
local mat3F = {
    { 0, 0, 0 },
    { 0, 0, 0 },
    { 0, 0, 0 }
}

---@return matrix4x4 matrix
function mat4(...)
    local inputs = { ... }

    local data
    if #inputs == 1 then
        data = inputs[1]
    else
        data = inputs
    end
    local matrix
    if #data == 16 then
        matrix = {}
        for i = 1, 4 do
            matrix[i] = {}
            for j = 1, 4 do
                matrix[i][j] = data[(j - 1) * 4 + i]
            end
        end
    elseif #data == 4 then -- 4x {x,y,z,w}
        matrix = {}
        for i = 1, 4 do
            matrix[i] = {}
            for j = 1, 4 do
                matrix[i][j] = data[i][j]
            end
        end
    else
        matrix = {
            { 1, 0, 0, 0 },
            { 0, 1, 0, 0 },
            { 0, 0, 1, 0 },
            { 0, 0, 0, 1 }
        }
    end
    setmetatable(matrix, mat4Mt)
    return matrix
end

---@return matrix3x3 matrix
function mat3(...)
    local m = { ... }
    local matrix
    if type(m[1]) == "table" then
        matrix = unpack(m)
    elseif #m == 9 then
        matrix = {}
        for i = 1, 3 do
            matrix[i] = {}
            for j = 1, 3 do
                matrix[i][j] = m[(i - 1) * 3 + j]
            end
        end
    else
        matrix = {
            { 1, 0, 0 },
            { 0, 0, 0 },
            { 0, 1, 1 }
        }
    end
    setmetatable(matrix, mat3Mt)
    return matrix
end

mat3F.type = "matrix_3x3"
mat4F.type = "matrix_4x4"
function mat4Mt.__tostring(self)
    local t = tostring
    local m = self
    local str = "[\n"
    str = str .. "   " .. t(m[1][1]) .. ", " .. t(m[2][1]) .. ", " .. t(m[3][1]) .. ", " .. t(m[4][1]) .. "\n"
    str = str .. "   " .. t(m[1][2]) .. ", " .. t(m[2][2]) .. ", " .. t(m[3][2]) .. ", " .. t(m[4][2]) .. "\n"
    str = str .. "   " .. t(m[1][3]) .. ", " .. t(m[2][3]) .. ", " .. t(m[3][3]) .. ", " .. t(m[4][3]) .. "\n"
    str = str .. "   " .. t(m[1][4]) .. ", " .. t(m[2][4]) .. ", " .. t(m[3][4]) .. ", " .. t(m[4][4]) .. "\n"
    return str .. "] "
end

function mat3Mt.__tostring(self)
    local t = tostring
    local m = self
    local str = "[\n"
    str = str .. "   " .. t(m[1][1]) .. ", " .. t(m[2][1]) .. ", " .. t(m[3][1]) .. "\n"
    str = str .. "   " .. t(m[1][2]) .. ", " .. t(m[2][2]) .. ", " .. t(m[3][2]) .. "\n"
    str = str .. "   " .. t(m[1][3]) .. ", " .. t(m[2][3]) .. ", " .. t(m[3][3]) .. "\n"
    return str .. "] "
end

function mat4F:vMul(vector)
    local m = self
    local x = m[1][1] * vector.x + m[2][1] * vector.y + m[3][1] * vector.z + m[4][1] * vector.w
    local y = m[1][2] * vector.x + m[2][2] * vector.y + m[3][2] * vector.z + m[4][2] * vector.w
    local z = m[1][3] * vector.x + m[2][3] * vector.y + m[3][3] * vector.z + m[4][3] * vector.w
    local w = m[1][4] * vector.x + m[2][4] * vector.y + m[3][4] * vector.z + m[4][4] * vector.w
    return vec4(x, y, z, w)
end

function mat3F:vMul(vector)
    local x = self[1][1] * vector.x + self[2][1] * vector.y + self[3][1] * vector.z
    local y = self[1][2] * vector.x + self[2][2] * vector.y + self[3][2] * vector.z
    local z = self[1][3] * vector.x + self[2][3] * vector.y + self[3][3] * vector.z
    return vec3(x, y, z)
end

function mat4Mt.__add(x, y)
    local v = mat4()
    if type(y) == "table" then
        for i = 1, 4 do
            for j = 1, 4 do
                v[i][j] = x[i][j] + y[i][j]
            end
        end
    else
        for i = 1, 4 do
            for j = 1, 4 do
                v[i][j] = x[i][j] + y
            end
        end
    end
    return v
end

function mat3Mt.__add(x, y)
    local v = mat3()
    if type(y) == "table" then
        for i = 1, 3 do
            for j = 1, 3 do
                v[i][j] = x[i][j] + y[i][j]
            end
        end
    else
        for i = 1, 3 do
            for j = 1, 3 do
                v[i][j] = x[i][j] + y
            end
        end
    end
    return v
end

mat4Mt.__index = mat4F

mat3Mt.__index = mat3F

function mat4Mt.__mul(x, y)
    local m = mat4()
    for i = 1, 4 do
        for j = 1, 4 do
            m[i][j] = x[i][1] * y[1][j] + x[i][2] * y[2][j] + x[i][3] * y[3][j] + x[i][4] * y[4][j]
        end
    end
    return m
end

function mat3Mt.__mul(x, y)
    local m = mat3()
    for i = 1, 3 do
        for j = 1, 3 do
            m[i][j] = x[i][1] * y[1][j] + x[i][2] * y[2][j] + x[i][3] * y[3][j]
        end
    end
    return m
end

function mat4Mt.__div(x, y)
    local v = mat4()
    for i = 1, 4 do
        for j = 1, 4 do
            v[i][j] = x[i][j] / y[i][j]
        end
    end
    return v
end

function mat3Mt.__div(x, y)
    local v = mat3()
    for i = 1, 3 do
        for j = 1, 3 do
            v[i][j] = x[i][j] / y[i][j]
        end
    end
    return v
end

function mat4Mt.__sub(x, y)
    local v = mat4()
    for i = 1, 4 do
        for j = 1, 4 do
            v[i][j] = x[i][j] - y[i][j]
        end
    end
    return v
end

function mat3Mt.__sub(x, y)
    local v = mat3()
    for i = 1, 3 do
        for j = 1, 3 do
            v[i][j] = x[i][j] - y[i][j]
        end
    end
    return v
end

function mat4Mt.__unm(x)
    local v = mat4()
    for i = 1, 4 do
        for j = 1, 4 do
            v[i][j] = -x[i][j]
        end
    end
    return v
end

function mat3Mt.__unm(x)
    local v = mat4()
    for i = 1, 3 do
        for j = 1, 3 do
            v[i][j] = -x[i][j]
        end
    end
    return v
end

function mat4F:transpose()
    local v = {}
    v[1] = { self[1][1], self[2][1], self[3][1], self[4][1] }
    v[2] = { self[1][2], self[2][2], self[3][2], self[4][2] }
    v[3] = { self[1][3], self[2][3], self[3][3], self[4][3] }
    v[4] = { self[1][4], self[2][4], self[3][4], self[4][4] }
    return setmetatable(v, mat4Mt)
end

function mat3F:transpose()
    local v = {}
    v[1] = { self[1][1], self[2][1], self[3][1] }
    v[2] = { self[1][2], self[2][2], self[3][2] }
    v[3] = { self[1][3], self[2][3], self[3][3] }
    return setmetatable(v, mat3Mt)
end

function mat4F:copy()
    local v = {}
    v[1] = { self[1][1], self[1][2], self[1][3], self[1][4] }
    v[2] = { self[2][1], self[2][2], self[2][3], self[2][4] }
    v[3] = { self[3][1], self[3][2], self[3][3], self[3][4] }
    v[4] = { self[4][1], self[4][2], self[4][3], self[4][4] }
    return setmetatable(v, mat4Mt)
end

function mat3F:copy()
    local v = {}
    v[1] = { self[1][1], self[1][2], self[1][3] }
    v[2] = { self[2][1], self[2][2], self[2][3] }
    v[3] = { self[3][1], self[3][2], self[3][3] }
    return setmetatable(v, mat3Mt)
end

function mat4F:table()
    return {
        self[1][1], self[1][2], self[1][3], self[1][4],
        self[2][1], self[2][2], self[2][3], self[2][4],
        self[3][1], self[3][2], self[3][3], self[3][4],
        self[4][1], self[4][2], self[4][3], self[4][4]
    }
end

function mat3F:table()
    return {
        self[1][1], self[1][2], self[1][3],
        self[2][1], self[2][2], self[2][3],
        self[3][1], self[3][2], self[3][3]
    }
end

-- translated from love12's github: https://github.com/love2d/love/blob/12.0-development/src/common/Matrix.cpp matrix4:invert
function mat4F:invert()
    local e = self:table()
    local inv = {}
    inv[1] = e[6] * e[11] * e[16] -
        e[6] * e[12] * e[15] -
        e[10] * e[7] * e[16] +
        e[10] * e[8] * e[15] +
        e[14] * e[7] * e[12] -
        e[14] * e[8] * e[11]

    inv[5] = -e[5] * e[11] * e[16] +
        e[5] * e[12] * e[15] +
        e[9] * e[7] * e[16] -
        e[9] * e[8] * e[15] -
        e[13] * e[7] * e[12] +
        e[13] * e[8] * e[11]

    inv[9] = e[5] * e[10] * e[16] -
        e[5] * e[12] * e[14] -
        e[9] * e[6] * e[16] +
        e[9] * e[8] * e[14] +
        e[13] * e[6] * e[12] -
        e[13] * e[8] * e[10]

    inv[13] = -e[5] * e[10] * e[15] +
        e[5] * e[11] * e[14] +
        e[9] * e[6] * e[15] -
        e[9] * e[7] * e[14] -
        e[13] * e[6] * e[11] +
        e[13] * e[7] * e[10]

    inv[2] = -e[2] * e[11] * e[16] +
        e[2] * e[12] * e[15] +
        e[10] * e[3] * e[16] -
        e[10] * e[4] * e[15] -
        e[14] * e[3] * e[12] +
        e[14] * e[4] * e[11]

    inv[6] = e[1] * e[11] * e[16] -
        e[1] * e[12] * e[15] -
        e[9] * e[3] * e[16] +
        e[9] * e[4] * e[15] +
        e[13] * e[3] * e[12] -
        e[13] * e[4] * e[11]

    inv[10] = -e[1] * e[10] * e[16] +
        e[1] * e[12] * e[14] +
        e[9] * e[2] * e[16] -
        e[9] * e[4] * e[14] -
        e[13] * e[2] * e[12] +
        e[13] * e[4] * e[10]

    inv[14] = e[1] * e[10] * e[15] -
        e[1] * e[11] * e[14] -
        e[9] * e[2] * e[15] +
        e[9] * e[3] * e[14] +
        e[13] * e[2] * e[11] -
        e[13] * e[3] * e[10]

    inv[3] = e[2] * e[7] * e[16] -
        e[2] * e[8] * e[15] -
        e[6] * e[3] * e[16] +
        e[6] * e[4] * e[15] +
        e[14] * e[3] * e[8] -
        e[14] * e[4] * e[7]

    inv[7] = -e[1] * e[7] * e[16] +
        e[1] * e[8] * e[15] +
        e[5] * e[3] * e[16] -
        e[5] * e[4] * e[15] -
        e[13] * e[3] * e[8] +
        e[13] * e[4] * e[7]

    inv[11] = e[1] * e[6] * e[16] -
        e[1] * e[8] * e[14] -
        e[5] * e[2] * e[16] +
        e[5] * e[4] * e[14] +
        e[13] * e[2] * e[8] -
        e[13] * e[4] * e[6]

    inv[15] = -e[1] * e[6] * e[15] +
        e[1] * e[7] * e[14] +
        e[5] * e[2] * e[15] -
        e[5] * e[3] * e[14] -
        e[13] * e[2] * e[7] +
        e[13] * e[3] * e[6]

    inv[4] = -e[2] * e[7] * e[12] +
        e[2] * e[8] * e[11] +
        e[6] * e[3] * e[12] -
        e[6] * e[4] * e[11] -
        e[10] * e[3] * e[8] +
        e[10] * e[4] * e[7]

    inv[8] = e[1] * e[7] * e[12] -
        e[1] * e[8] * e[11] -
        e[5] * e[3] * e[12] +
        e[5] * e[4] * e[11] +
        e[9] * e[3] * e[8] -
        e[9] * e[4] * e[7]

    inv[12] = -e[1] * e[6] * e[12] +
        e[1] * e[8] * e[10] +
        e[5] * e[2] * e[12] -
        e[5] * e[4] * e[10] -
        e[9] * e[2] * e[8] +
        e[9] * e[4] * e[6]

    inv[16] = e[1] * e[6] * e[11] -
        e[1] * e[7] * e[10] -
        e[5] * e[2] * e[11] +
        e[5] * e[3] * e[10] +
        e[9] * e[2] * e[7] -
        e[9] * e[3] * e[6]

    local det = e[1] * inv[1] + e[2] * inv[5] + e[3] * inv[9] + e[4] * inv[13];

    local invdet = 1.0 / det

    for i = 1, 16 do
        inv[i] = inv[i] * invdet
    end

    return mat4(inv)
end

-- translated from love12's github: https://github.com/love2d/love/blob/12.0-development/src/common/Matrix.cpp
function mat3F:invert()
    local e = self:table()
    -- e0 e3 e6
    -- e1 e4 e7
    -- e2 e5 e8

    local det = e[1] * (e[5] * e[9] - e[8] * e[6]) - e[2] * (e[4] * e[9] - e[6] * e[7]) +
        e[3] * (e[4] * e[8] - e[5] * e[7])

    local invdet = 1 / det

    local m = {}

    m[1] = invdet * (e[5] * e[9] - e[8] * e[6])
    m[4] = -invdet * (e[2] * e[9] - e[3] * e[8])
    m[7] = invdet * (e[2] * e[6] - e[3] * e[5])
    m[2] = -invdet * (e[4] * e[9] - e[6] * e[7])
    m[5] = invdet * (e[1] * e[9] - e[3] * e[7])
    m[8] = -invdet * (e[1] * e[6] - e[4] * e[3])
    m[3] = invdet * (e[4] * e[8] - e[7] * e[5])
    m[6] = -invdet * (e[1] * e[8] - e[7] * e[2])
    m[9] = invdet * (e[1] * e[5] - e[4] * e[2])

    return mat3(unpack(m))
end

--- set matrix4x4
---@param mat4 matrix4x4
function mat4F:set(mat4)
    self[1][1], self[1][2], self[1][3], self[1][4] = mat4[1][1], mat4[1][2], mat4[1][3], mat4[1][4]
    self[2][1], self[2][2], self[2][3], self[2][4] = mat4[2][1], mat4[2][2], mat4[2][3], mat4[2][4]
    self[3][1], self[3][2], self[3][3], self[3][4] = mat4[3][1], mat4[3][2], mat4[3][3], mat4[3][4]
    self[4][1], self[4][2], self[4][3], self[4][4] = mat4[4][1], mat4[4][2], mat4[4][3], mat4[4][4]
end

--- set matrix3x3
--- @param mat3 matrix3x3
function mat3F:set(mat3)
    self[1][1], self[1][2], self[1][3] = mat3[1][1], mat3[1][2], mat3[1][3]
    self[2][1], self[2][2], self[2][3] = mat3[2][1], mat3[2][2], mat3[2][3]
    self[3][1], self[3][2], self[3][3] = mat3[3][1], mat3[3][2], mat3[3][3]
end
