ffi = require("ffi")

require("love.image")

local gltfLoader = require("Rhodium.Modules.Models.asyncGLTF")

local mountPath, arguments, loadFunctionKey = ...

if mountPath then
    assert(love.filesystem.mountFullPath(mountPath, "Model", "read"), "Failed to mount folder")
end

local data = gltfLoader[loadFunctionKey](unpack(arguments))

if mountPath then
    love.filesystem.unmountFullPath(mountPath)
end

return data
