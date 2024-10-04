local indexedTableMetatable = {}
---@class iDIndexedTable
---@field indexTable table
---@field items table
---@field key string
local iDIndexedTableFunctions = { indexTable = {}, items = {} }

---Creates a new indexed table
---@param key? string
---@return iDIndexedTable
function newIdIndexedTable(key)
    local t = {
        indexTable = {},
        items = {},
        key = key or "id",
    }
    setmetatable(t, indexedTableMetatable)
    return t
end

indexedTableMetatable.__index = function(t, k)
    return iDIndexedTableFunctions[k] or t.items[k]
end

indexedTableMetatable.__len = function(t)
    return #t.items
end

local objectIndexedTableMetatable = {}
---@class objectIndexedTable
---@field indexTable table
local objectIndexedTableFunctions = { indexTable = {}, items = {} }

---@return objectIndexedTable
function newObjectIndexedTable()
    return setmetatable({ indexTable = {}, items = {} }, objectIndexedTableMetatable)
end

objectIndexedTableMetatable.__index = function(t, k)
    return objectIndexedTableFunctions[k] or t.items[k]
end

objectIndexedTableMetatable.__len = function(t)
    return #t.items
end

function iDIndexedTableFunctions:add(v)
    assert(v[self.key], "Object doesn't have an id")
    assert(not self.indexTable[v[self.key]], "Object with id " .. v[self.key] .. " already exists")
    table.insert(self.items, v)
    self.indexTable[v[self.key]] = #self.items
end

function iDIndexedTableFunctions:get(id)
    return self.items[self.indexTable[id]]
end

---removes something from the table
---@param index number
function iDIndexedTableFunctions:remove(index)
    -- get the object at the index
    local w = self.items[index]

    -- if the object is the last object in the table, we can just remove it
    if index == #self.items then
        table.remove(self.items, index)
        self.indexTable[w[self.key]] = nil
    else
        -- get the index and object of the last object in the table
        local lastIndex = #self.items
        local lastObject = self.items[lastIndex]

        -- swap the object at the index with the last object
        self.items[index] = lastObject
        self.indexTable[lastObject[self.key]] = index

        -- remove the last object
        self.indexTable[w[self.key]] = nil
        table.remove(self.items, #self.items)
    end
end

function iDIndexedTableFunctions:removeAsObject(v)
    -- if the object is valid and has an id
    if v and v[self.key] then
        local index = self.indexTable[v[self.key]]

        -- if the object is in the table
        if index then
            -- if the object is the last object in the table, we can just remove it
            if index == #self.items then
                self.indexTable[v[self.key]] = nil
                return table.remove(self.items, index)
            else
                -- get the index and object of the last object in the table
                local lastObject = self.items[#self.items]
                self.items[index] = lastObject

                -- if the last object has a valid id, update the index table
                if lastObject then
                    self.indexTable[lastObject[self.key]] = index
                end

                -- remove the object from the index table and the table
                self.indexTable[v[self.key]] = nil
                return table.remove(self.items, #self.items)
            end
        end
    end
end

--- removes an object from the table by id
---@param id any
---@return any
function iDIndexedTableFunctions:removeById(id)
    -- if the id is valid
    if id then
        return self:remove(self.indexTable[id])
    end
end

function objectIndexedTableFunctions:add(v)
    table.insert(self.items, v)
    self.indexTable[v] = #self.items
end

---removes something from the table
---@param i number
function objectIndexedTableFunctions:remove(i)
    local w = self.items[i]
    if i == #self.items then
        self.indexTable[w] = nil
        return table.remove(self.items, i)
    else
        local lastIndex = #self.items
        local lastObject = self.items[lastIndex]
        self.items[i] = lastObject
        self.indexTable[lastObject] = i
        self.indexTable[w] = nil
        return table.remove(self.items, #self.items)
    end
end

function objectIndexedTableFunctions:removeAsObject(v)
    assert(v, "Object is nil")
    local i = self.indexTable[v]
    if i then
        if i == #self.items then
            self.indexTable[v] = nil
            table.remove(self.items)
        else
            local lastObject = self.items[#self.items]
            self.items[i] = lastObject
            if lastObject then
                self.indexTable[lastObject] = i
            end
            self.indexTable[v] = nil
            table.remove(self.items)
        end

        return true
    end

    return false
end

function objectIndexedTableFunctions:clear()
    self.items = {}
    self.indexTable = {}
end

function iDIndexedTableFunctions:clear()
    self.items = {}
    self.indexTable = {}
end

function objectIndexedTableFunctions:setKey(object, key)
    local index = self.indexTable[object]
    if index then
        self.indexTable[object] = nil
        self.indexTable[key] = index
    end
end

function iDIndexedTableFunctions:setKey(object, key)
    local index = self.indexTable[object[self.key]]
    if index then
        self.indexTable[object[self.key]] = nil
        self.indexTable[key] = index
    end
end
