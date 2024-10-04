local loaderFunctions = require("Models.loader")

Rhodium.graphics.loadGltfFile, Rhodium.graphics.loadObjFile = loaderFunctions[1], loaderFunctions[2]
local asyncFunctions = require("Models.asyncGLTF")

local function loadGltfFileAsync(path, mountPath)
    local data, buffers

    local task = Rhodium.task.newAsyncTask(loadGLTF, { mountPath, { "Model/" .. path }, "readFile" }, function(result)
        data = result[1]
        buffers = result[2]
    end)

    Rhodium.task.await(task)

    -------- Load parts of the glTF file in parallel --------

    local group = Rhodium.task.setNewTaskGroup()

    local images, skippedWhileLoading, textures
    images, skippedWhileLoading, textures = asyncFunctions.parseTextures(mountPath, "Model/" .. path, data, buffers,
        loadGLTF)

    local skins
    Rhodium.task.newTask(loadGLTF, { mountPath, { data, buffers }, "parseSkins" }, function(result)
        skins = result
    end)

    local meshes
    Rhodium.task.newTask(loadGLTF, { mountPath, { data, buffers }, "parseMeshes" }, function(result)
        meshes = result
    end)

    local animations
    Rhodium.task.newTask(loadGLTF, { mountPath, { data, buffers }, "parseAnimations" }, function(result)
        animations = result
    end)


    local lights
    Rhodium.task.newTask(loadGLTF, { mountPath, { data }, "parseLights" }, function(result)
        lights = result
    end)

    -------- Wait for all tasks to finish --------

    Rhodium.task.await(group)

    -- load the textures that were skipped while loading the rest of the glTF file

    for i, data in ipairs(skippedWhileLoading) do
        textures[data.index] = {
            image = images[data.source + 1],
            sampler = data.sampler,
        }
    end

    -- load the materials

    local materials = {}
    if data.materials then
        for i, v in ipairs(data.materials) do
            materials[i] = asyncFunctions.createMaterial(textures, v)
        end
    end

    data = {
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

    meshes = {}

    local function meshCreationCallback(mesh)
        mesh.position = vec3(mesh.position)
        mesh.rotation = quaternion(mesh.rotation)
        mesh.scale = vec3(mesh.scale)

        table.insert(meshes, mesh)
    end

    local meshToNode, lightToNode, childToParent = loaderFunctions.processNodes(data)

    for meshIndex, mesh in ipairs(data.meshes) do
        for primitiveIndex, primitive in ipairs(mesh.primitives) do
            meshCreationCallback(loaderFunctions.processMesh(meshToNode, primitive, data, meshIndex))
        end
    end

    local animation = nil
    if #data.animations > 0 then
        animation = Rhodium.animation.loadAnimation(data)
    end

    lights = loaderFunctions.processLights(lightToNode, data)

    return meshes, animation, lights, data
end

--- Load a glTF file asynchronously
---@param path string The path to the file
---@param mountPath string The path to the directory containing the file (not required if it's a path that is mounted by default)
---@param doneCallback function? A callback to call when the file is loaded
---@param busyCallback function? A callback to call while the file is being loaded
---@return Rhodium.wrappedAsyncFunction? A function that can be called to continue loading the file or nil if there was an error
function Rhodium.graphics.loadGltfFileAsync(path, mountPath, doneCallback, busyCallback)
    local wrapped = Rhodium.task.wrapAsyncFunction(loadGltfFileAsync, doneCallback, busyCallback, path, mountPath)

    return wrapped
end
