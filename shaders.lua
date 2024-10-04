local defaultIncludePaths = {
    "Rhodium/Graphics/Shaders/",
    "Rhodium/Graphics/Shaders/VolumetricLighting/",
    "Rhodium/Graphics/Shaders/PostProcessing/",
    "Rhodium/Graphics/Shaders/PostProcessing/Bloom/",
    "Rhodium/Graphics/Shaders/AmbientOcclusion/",
    "Rhodium/Graphics/Shaders/ScreenSpaceReflections/",
    "Rhodium/Graphics/Shaders/Cubemaps/",
}

Rhodium.internal.shaders = {}
Rhodium.internal.computeShaders = {}

local colors = {
    ["red"] = "\27[31m",
    ["green"] = "\27[32m",
    ["yellow"] = "\27[33m",
    ["blue"] = "\27[34m",
    ["magenta"] = "\27[35m",
    ["cyan"] = "\27[36m",
    ["white"] = "\27[37m",
    ["reset"] = "\27[0m"
}

---@class  Rhodium.shader
---@field shader love.Shader
---@field name string
---@field data table
---@field totalLines integer
---@field includedFiles table
---@field fileName string
---@field uniforms table
local shaderFunctions = {}
local shaderMetaTable = {
    __index = shaderFunctions
}

Rhodium.internal.iDShaders = newIdIndexedTable()

local function coloredString(str, color)
    return colors[color] .. str .. colors["reset"]
end

function Rhodium.internal.addDefaultIncludePath(path)
    table.insert(defaultIncludePaths, path)
end

local function lines(str)
    ---@type integer|nil
    local pos = 1;
    return function()
        if not pos then return nil end
        local p1, p2 = string.find(str, "\n", pos, true)
        local line
        if p1 then
            line = str:sub(pos, p1 - 1)
            pos = p2 + 1
        else
            line = str:sub(pos)
            pos = nil
        end
        return line
    end
end

local function addLineToPreviousIncludes(t, lines)
    for i, v in ipairs(t) do
        v[2] = v[2] + lines
        v[3] = v[3] + lines
        addLineToPreviousIncludes(v[4], lines)
    end
end

local cachedShaders = {}

local function loadShaderFile(shaderCode, fileName, depth, enableCache, compileVariables)
    if depth > 20 then
        error("shader: [ " ..
            fileName .. " ] compilation failed\n" .. "too many includes, did you write a recursive include?", 2)
    end

    if cachedShaders[fileName] and enableCache then
        return
            cachedShaders[fileName].file,
            cachedShaders[fileName].data,
            cachedShaders[fileName].totalLines
    end

    local found = false
    local iterator

    local name, finalFile = "", ""

    if not shaderCode then
        -- shader provided as filename

        -- check if the file exists in any of the default include paths
        for i = 0, #defaultIncludePaths do
            local tempName

            -- if i == 0, then start at the root directory
            if i == 0 then
                tempName = fileName
            else
                tempName = defaultIncludePaths[i] .. fileName
            end


            if love.filesystem.getInfo(tempName) ~= nil then
                local success, tempIterator = pcall(love.filesystem.lines, tempName)
                if not success then
                    print("couldn't find shader file under: " .. tempName)
                end
                if found then
                    error("shader: [ " ..
                        tempName ..
                        " ] compilation failed\n" ..
                        "double include or two shaders with the same name under a different default filepath: " ..
                        fileName,
                        2)
                end
                name = tempName
                iterator = tempIterator
                found = true
            end
        end
    else
        -- shader provided as string

        iterator = lines(shaderCode)
        name = fileName
        found = true
    end

    local shaderData = { name, 1, 1, {} } -- {name, startLine, endLine, {includedFiles}}
    local lineIndex = 0
    local words = {}

    -- if the shader file was not found, return an error
    if not found then
        print("couldn't find shader file: [ " .. name .. " ] compilation failed")
        finalFile = finalFile .. "couldn't find shader file: [ " .. name .. " ] compilation failed\n"
        goto continue
    end


    for line in iterator do
        table.clear(words)
        for word in string.gmatch(line, "%S+") do
            table.insert(words, word)
        end

        if words[1] == "#include" then
            local includeFileName = string.match(words[2], '[^"]+')
            local shaderLines, includedFileData, lineAmount = loadShaderFile(nil, includeFileName, depth + 1, enableCache,
                compileVariables)
            -- lines, data about the included file {name, startLine, endLine, {includedFiles}} within the included file,
            -- amount of lines in the included file(s)
            finalFile = finalFile .. shaderLines .. "\n"

            local includedShaders = shaderData[4] -- get included files table

            -- check for double includes

            for i, v in ipairs(includedShaders) do
                if v[1] == includeFileName then
                    error("shader: [ " .. name .. " ] compilation failed\n" .. "double include: " .. includeFileName, 2)
                end
            end

            -- add included file data to the current file data
            table.insert(includedShaders, includedFileData) -- add included file data to the current file data

            -- add the current line index to the included file data
            addLineToPreviousIncludes({ includedFileData }, lineIndex) -- add the current line index to the included file data

            lineIndex = lineIndex + lineAmount
            -- increase line index by the amount of lines in the included file
        elseif words[1] == "#defineExtern" then
            -- command is wrapped in a string so we can use spaces, so only use stuff after the first " and before the last "
            local afterFirstQuote = line:sub(line:find('"', nil, true) + 1)
            local key = afterFirstQuote:sub(1, afterFirstQuote:find('"', nil, true) - 1)

            local variableName = words[2]

            Rhodium.internal.assert(variableName)

            local value = compileVariables[key]

            if value == nil then
                value = Rhodium.internal.defaultShaderCompileVariables[key]
            end

            Rhodium.internal.assert(value, "Shader compilation failed due to const define variable [ " .. key .. " ]")

            local newLine = "#define " .. variableName .. " " .. value

            finalFile = finalFile .. newLine .. "\n"
        elseif words[1] == "#defineExternIf" then
            -- command is wrapped in a string so we can use spaces, so only use stuff after the first " and before the last "
            local afterFirstQuote = line:sub(line:find('"', nil, true) + 1)
            local command = afterFirstQuote:sub(1, afterFirstQuote:find('"', nil, true) - 1)

            local name = words[2]
            local ret, err = loadstring("return " .. command)
            if not ret then
                error("Shader compilation failed due to const define variable [ " .. name .. " ].\nError: " .. err)
            end
            Rhodium.internal.assert(name)
            local returned = ret()
            if returned then
                local newLine = "#define " .. name .. " 1"
                finalFile = finalFile .. newLine .. "\n"
            end
        else
            finalFile = finalFile .. line .. "\n"
        end
        lineIndex = lineIndex + 1
    end
    ::continue::

    shaderData[3] = lineIndex

    if depth ~= 0 then
        -- don't cache the main shader file
        cachedShaders[fileName] = {
            file = finalFile,
            data = shaderData,
            totalLines = lineIndex
        }
    end

    return finalFile, shaderData, lineIndex
end

local function findErrorFile(t, line)
    for _, v in ipairs(t) do
        if line >= v[2] and line <= v[3] then
            local fileName, startLine, endLine, included = findErrorFile(v[4], line)
            if fileName then
                return fileName, startLine, endLine, included
            end
            return v[1], v[2], v[3], v[4]
        end
    end
end

local function findErrorLine(err, includedFiles, shaderCode, name, errorPos)
    local i = 0
    local prevLine = ""
    local errorLine = ""
    if errorPos == -1 then error("shader: " .. name .. "\n" .. err, 2) end
    -- find the line before the error line and the error line
    for line in lines(shaderCode) do
        i = i + 1
        if i == errorPos - 1 then
            prevLine = line
        end
        if i == errorPos then
            errorLine = line
            break
        end
    end

    local fileName, startLine, endLine, included = findErrorFile({ includedFiles }, errorPos)
    -- subtract all included files from the errorPos that are before the errorPos in the file

    -- catch #ifdef / #endif's that weren't closed, the error won't be on any lines in any file
    if not included then
        error("shader: [ " .. name .. " ] compilation failed\n" .. "couldn't find error in file" .. "\n" ..
            err, 3)
    end

    local newErrorPos = errorPos
    for i, v in ipairs(included) do
        if v[3] < errorPos then
            newErrorPos = newErrorPos - (v[3] - v[2]) - 1
        end
    end
    errorPos = newErrorPos - startLine + 1
    return errorPos, prevLine, errorLine, fileName
end

Rhodium.internal.defaultShaderUniforms = {}
Rhodium.internal.defaultShaderCompileVariables = {}

function Rhodium.internal.removeDefaultShaderUniform(name)
    for i, data in ipairs(Rhodium.internal.defaultShaderUniforms) do
        if data[1] == name then
            table.remove(Rhodium.internal.defaultShaderUniforms, i)
            break
        end
    end
end

function Rhodium.internal.addDefaultShaderUniform(...)
    table.insert(Rhodium.internal.defaultShaderUniforms, { ... })

    for i, shader in pairs(Rhodium.internal.shaders) do
        if shader:hasUniform(select(1, ...)) then
            shader:send(...)
        end
    end

    for i, shader in pairs(Rhodium.internal.computeShaders) do
        if shader:hasUniform(select(1, ...)) then
            shader:send(...)
        end
    end
end

function Rhodium.internal.setDefaultShaderCompileVariable(key, value)
    Rhodium.internal.defaultShaderCompileVariables[key] = value
end

function Rhodium.graphics.compileShaders()
    local totalLinesCompiled = 0
    local totalShadersCompiled = 0

    cachedShaders = {}

    local newShader = Rhodium.graphics.newShader
    local newCompute = Rhodium.graphics.newComputeShader

    Rhodium.internal.shaders = {
        main                   = newShader("main.glsl", { "Main", reflections = 1, enableCache = false }),
        mainNoReflections      = newShader("main.glsl", { "Main No Reflections", reflections = 0, enableCache = false }),

        transparency           = newShader("transparency.glsl", "Transparency"),
        screenSpaceReflections = newShader("reflections.glsl", "Screen space reflections"),
        shadows                = newShader("shadows.glsl", "Shadow mapping"),
        basic                  = newShader("basicVert.glsl", "Basic vertex"),
        transparentShadows     = newShader("transparentShadows.glsl", "Transparent shadow mapping"),

        material               = newShader("material.glsl", { "Material", instanced = 0, animated = 0 }),
        materialsAnimated      = newShader("material.glsl", { "Material animated", instanced = 0, animated = 1 }),
        materialsInstanced     = newShader("material.glsl", { "Material instanced", instanced = 1, animated = 0 }),

        gridDrawer             = newShader("grid.glsl", "Grid"),
        irradianceMapping      = newShader("irradianceMapping.glsl", "Irradiance mapping"),
        instancedDrawing       = newShader("instancedDrawer.glsl", "Instanced draw"),
        bloomThreshold         = newShader("threshold.glsl", "Bloom Threshold"),
        bloomDownsample        = newShader("downsample.glsl", "Bloom Downsample"),
        bloomUpsample          = newShader("upsample.glsl", "Bloom Upsample"),
        postProcessing         = newShader("postProcessing.glsl", "post processing"),
        chromaticAberration    = newShader("chromaticAberration.glsl", "Chromatic Aberration"),

        autoExposure           = newShader("draw1x1Mip.glsl", "Exposure"),
        outline                = newShader("outline.glsl", "Outline"),

        blurDepthMin           = newShader("blurDepthMin.glsl", "Blur Depth Min"),
        drawAtMip              = newShader("drawAtMip.glsl", "Bloom Drawer"),
        specularFilter         = newShader("specularMapping.glsl", "Specular Filter"),
        cubemapDrawer          = newShader("cubemapDrawer.glsl", "Cubemap Drawer"),
        cubemaps               = newShader("cubemaps.glsl", "Cubemaps"),
        overrideDepth          = newShader("overrideDepth.glsl", "Override Depth"),
        exposure               = newShader("exposure.glsl", "Exposure"),
        cubemapToOctahedral    = newShader("cubemapToOctahedral.glsl", "Cubemap to Octahedral"),
        reflectionsPostProcess = newShader("reflectionsPostProcess.glsl", "Reflections Post Process"),
        volumetricApply        = newShader("volumetricApply.glsl", { validateES = false }),
        volumetricOcclusion    = newShader("volumetricOcclusion.glsl"),
        depthPrepassOpaque     = newShader("depthPrepass.glsl", { "Depth Prepass Opaque", opaque = 1 }),
        depthPrepassAlphaMask  = newShader("depthPrepass.glsl", { "Depth Prepass Alpha Mask", opaque = 0 }),

        writeChannelToAll      = newShader("writeChannelToAll.glsl"),
        meshID                 = newShader("meshID.glsl", { "Mesh ID", instanced = 0 }),
        meshIDInstanced        = newShader("meshID.glsl", { "Mesh ID Instanced", instanced = 1 }),
        billboard              = newShader("billboard.glsl"),
    }

    Rhodium.internal.computeShaders = {
        sampleVolumes     = newCompute("sampleVolumes.glsl"),
        downsampleVolumes = newCompute("volumetricDownsample.glsl", { "Downsample Volumes", validateES = false }),
        denoiseGTAO       = newCompute("gtaoDenoise.glsl"),
        depthPrepassGTAO  = newCompute("gtaoDepthPrepass.glsl"),
        gtao              = newCompute("gtao.glsl"),
        gtaoTAA           = newCompute("gtaoTAA.glsl"),
    }

    -- print("Compiled " .. totalShadersCompiled)
    -- print("Total lines compiled: " .. totalLinesCompiled)
    -- print("Compilation time: " .. love.timer.getTime() - startTime)
    -- print("Average line count: " .. totalLinesCompiled / totalShadersCompiled)

    Rhodium.internal.precalculateShaderData()

    for _, scene in ipairs(Rhodium.scene.getScenes()) do
        for i, light in ipairs(scene.pointLights.items) do
            light:applyChanges()
        end

        for i, light in ipairs(scene.directionalLights.items) do
            light:applyChanges()
        end

        for i, light in ipairs(scene.spotLights.items) do
            light:applyChanges()
        end

        for i, light in ipairs(scene.areaLights.items) do
            light:applyChanges()
        end
    end

    local graphicsData = Rhodium.internal.graphicsData

    if graphicsData and graphicsData.pointDrawer then
        graphicsData.pointDrawer:setShader(Rhodium.internal.shaders.instancedDrawing)
        graphicsData.overlayPointDrawer:setShader(Rhodium.internal.shaders.instancedDrawing)
        graphicsData.lineDrawer:setShader(Rhodium.internal.shaders.instancedDrawing)
        graphicsData.overlayLineDrawer:setShader(Rhodium.internal.shaders.instancedDrawing)
    end

    for i, shader in pairs(Rhodium.internal.shaders) do
        for i, data in ipairs(Rhodium.internal.defaultShaderUniforms) do
            if shader:hasUniform(data[1]) then
                shader:send(unpack(data))
            end
        end
    end

    for i, shader in pairs(Rhodium.internal.computeShaders) do
        for i, data in ipairs(Rhodium.internal.defaultShaderUniforms) do
            if shader:hasUniform(data[1]) then
                shader:send(unpack(data))
            end
        end
    end

    if Rhodium.internal.updateCameraSettings then
        Rhodium.internal.updateCameraSettings()
    end

    -- check if we've loaded
    if Rhodium.scene.getCurrentScene() and Rhodium.internal.iblRoughnessOneLevel then
        for i, camera in ipairs(Rhodium.scene.getCurrentScene().cameras.items) do
            ---@cast camera Rhodium.camera
            camera:recalculateProjectionMatrix()
            camera:updateShaderUniforms()
        end

        Rhodium.internal.updateShaderVariables(Rhodium.scene.getCurrentScene().engineData.settings)

        Rhodium.internal.shaders.cubemaps:send("SpecularTextures", Rhodium.internal.specularMaps)
        Rhodium.internal.shaders.cubemaps:send("IrradianceTextures", Rhodium.internal.irradianceMaps)

        Rhodium.internal.shaders.transparency:send("SpecularTextures", Rhodium.internal.specularMaps)
        Rhodium.internal.shaders.transparency:send("IrradianceTextures", Rhodium.internal.irradianceMaps)

        Rhodium.internal.sendLightBuffers()

        Rhodium.internal.materialRenderPass.shader = Rhodium.internal.shaders.material
        Rhodium.internal.shadowmapRenderPass.shader = Rhodium.internal.shaders.shadows
        Rhodium.internal.coloredLightRenderPass1.shader = Rhodium.internal.shaders.transparentShadows
        Rhodium.internal.coloredLightRenderPass2.shader = Rhodium.internal.shaders.transparentShadows
        Rhodium.internal.transparencyRenderPass.shader = Rhodium.internal.shaders.transparency
        Rhodium.internal.directionalLightRenderPass.shader = Rhodium.internal.shaders.shadows
        Rhodium.internal.opaqueDepthPrepassRenderPass.shader = Rhodium.internal.shaders.depthPrepassOpaque
        Rhodium.internal.alphaMaskedDepthPrepassRenderPass.shader = Rhodium.internal.shaders.depthPrepassAlphaMask

        Rhodium.internal.getCurrentCamera():use()
    end
end

function Rhodium.internal.precalculateShaderData()
    local numKernels = 64
    local kernels = {}
    local angle = math.pi * (3 - math.sqrt(5)) -- Golden angle in radians
    local n = numKernels                       -- Number of vertices

    for i = 0, n - 1 do
        local z = 1 - (i / n) * 2           -- Elevation
        local radius = math.sqrt(1 - z * z) -- Distance from the z-axis

        local azimuth = angle * i

        local x = math.cos(azimuth) * radius
        local y = math.sin(azimuth) * radius

        -- Now (x, y, z) is a point on the hemisphere
        local scale = i / numKernels
        scale = Rhodium.math.mix(0.1, 1.0, scale * scale)
        local sample = Temp.Vec3(x, y, math.abs(z)) * scale

        table.insert(kernels, sample:table())
    end
end

function Rhodium.internal.calculateGaussianKernel(kernelSize)
    local sigma = kernelSize * 0.25
    local kernel = {}
    local sum = 0
    for y = -kernelSize / 2, kernelSize / 2 do
        for x = -kernelSize / 2, kernelSize / 2 do
            local value = (1 / (2 * 3.14159265359 * sigma * sigma)) * math.exp(-((x * x + y * y) / (2 * sigma * sigma)))
            sum = sum + value
            kernel[(y + 2) * kernelSize + (x + 2)] = value
        end
    end
    for i = 1, kernelSize * kernelSize do
        kernel[i] = kernel[i] / sum
    end
    return kernel
end

Rhodium.internal.errorOnShaderFailure = true
local defaultShaderPaths = {}

local totalLinesCompiled = 0
local totalShadersCompiled = 0

local function preprocessShader(name, options, compileVariables, esValidation)
    local providedFilename = string.sub(name, -5) == ".glsl"

    options = options or {}

    do
        if not love.filesystem.getInfo(name) and providedFilename then
            for i = 1, #defaultShaderPaths do
                if love.filesystem.getInfo(defaultShaderPaths[i] .. name) then
                    name = defaultShaderPaths[i] .. name
                    break
                end
            end
        end
    end

    local shaderCode, includedFiles, totalLines

    if providedFilename then
        shaderCode, includedFiles, totalLines = loadShaderFile(nil, name, 0, options.enableCache ~= false,
            compileVariables)
    else
        shaderCode, includedFiles, totalLines = loadShaderFile(name, options.debugname, 0, options.enableCache ~= false,
            compileVariables)
    end

    totalLinesCompiled = totalLinesCompiled + totalLines
    totalShadersCompiled = totalShadersCompiled + 1

    local status, warning = love.graphics.validateShader(esValidation, shaderCode)
    if not status then
        -- if the shader failed to compile, get error info

        -- create shader file, without file caches, since that messes up the error line
        if providedFilename then
            shaderCode, includedFiles, totalLines = loadShaderFile(nil, name, 0, false, compileVariables)
        else
            shaderCode, includedFiles, totalLines = loadShaderFile(name, options.debugname, 0, false,
                compileVariables)
        end

        status, warning = love.graphics.validateShader(esValidation, shaderCode)

        if not providedFilename then
            if Rhodium.internal.errorOnShaderFailure then
                error("[NoTraceback]\nShader: " .. coloredString('"' .. name, "green") .. "\n\n" ..
                    warning, 2)
            else
                print("\nShader: " .. name .. "\n\n" .. warning, 2)
            end
        end

        -- error in the combined shader file
        local globalErrorPos = 0
        local i = 0
        for line in lines(warning) do
            i = i + 1

            -- the error line occurs on the 3rd line of the warning
            if i == 3 then
                local words = {}
                for word in line:gmatch "([^%s]+)" do
                    table.insert(words, word)
                end
                local pos = words[2]:gsub(":", "")
                globalErrorPos = tonumber(pos) or -1
                break
            end
        end

        -- get the file and the error line relative to the file
        local errorPos, prevLine, errorLine, fileName = findErrorLine(warning, includedFiles,
            shaderCode, name, globalErrorPos)

        local errorName = name

        if fileName ~= name then
            errorName = fileName
        end

        if Rhodium.internal.errorOnShaderFailure then
            error("[NoTraceback]\nShader: " .. coloredString('"' .. errorName .. ":" ..
                    errorPos, "green") .. "\n\n" .. (fileName ~= name and ("Included in " .. name .. "\n") or "") ..
                coloredString("previous line, " .. (errorPos - 1) .. ": " .. prevLine .. "\n" ..
                    "error line, " .. errorPos .. ": " .. errorLine, "cyan") .. "\n" .. "\n" ..
                warning, 2)
        else
            print("\nShader: " .. errorName .. ":" ..
                errorPos .. "\n\n" .. (fileName ~= name and ("Included in " .. name .. "\n") or "") ..
                "previous line, " .. (errorPos - 1) .. ": " .. prevLine .. "\n" ..
                "error line, " .. errorPos .. ": " .. errorLine .. "\n" .. "\n" ..
                warning, 2)
        end
    end

    return shaderCode
end

--- Creates a new shader object from a file or a string.
---@param name string filepath or string of the shader
---@param options? {[1]: string?, debugname: string, enableCache:boolean, id?:string, validateES?: boolean}|string options for the shader or the debugname
---@return Rhodium.shader
local function newShader(name, options, isComputeShader)
    local t = type(options)

    if t == "nil" then
        options = {}
    elseif t == "string" then
        options = { debugname = options }
    elseif t == "table" then
        if not options.debugname then
            options.debugname = options[1]
        end
    else
        error("Invalid options type: " .. t)
    end

    local esValidation = not isComputeShader

    if options.validateES ~= nil then
        esValidation = options.validateES
    end

    local shaderCode = preprocessShader(name, options, options, esValidation)

    local id = options.id or ((options.debugname or "") .. name)
    local shader

    if isComputeShader then
        shader = love.graphics.newComputeShader(shaderCode, options)
    else
        shader = love.graphics.newShader(shaderCode, options)
    end

    if shader then
        local other = Rhodium.internal.iDShaders:get(id)

        local self = {
            shader = shader,
            id = id,
            name = options.debugname or name,
            data = shaderCode,
            totalLines = totalLinesCompiled,
            includedFiles = {},
            fileName = name,
            uniforms = {},
            type = "shader",
            onReloadFunctions = other and other.onReloadFunctions or {}
        }

        setmetatable(self, shaderMetaTable)

        if other then
            Rhodium.internal.iDShaders:removeAsObject(other)

            for i, func in ipairs(other.onReloadFunctions) do
                func(self)
            end

            other:destroy()
        end

        Rhodium.internal.iDShaders:add(self)

        return self
    else
        return Rhodium.internal.iDShaders:get(id)
    end
end

--- Creates a new shader object from a file or a string.
---@param name string filepath or string of the shader
---@param options? {[1]: string?, debugname: string, enableCache:boolean, id?:string, validateES?: boolean}|string options for the shader or the debugname
---@return Rhodium.shader
function Rhodium.graphics.newShader(name, options)
    return newShader(name, options, false)
end

--- Creates a new compute shader object from a file or a string.
--- @param name string filepath or string of the shader
--- @param options? {[1]: string?, debugname: string, enableCache:boolean, id?:string, validateES?: boolean}|string options for the shader
--- @return Rhodium.shader
function Rhodium.graphics.newComputeShader(name, options)
    return newShader(name, options, true)
end

function shaderFunctions:destroy()
    self.shader:release()
end

function shaderFunctions:send(name, ...)
    self.shader:send(name, ...)
end

function shaderFunctions:sendColor(name, color)
    self.shader:send(name, color[1], color[2], color[3], color[4])
end

local tempTableVec2 = { 0, 0 }

--- Sends a vec2 to the shader
---@param name string
---@param vec vec2
function shaderFunctions:sendVec2(name, vec)
    tempTableVec2[1] = vec.x
    tempTableVec2[2] = vec.y

    self.shader:send(name, tempTableVec2)
end

local tempTableVec3 = { 0, 0, 0 }

--- Sends a vec3 to the shader
--- @param name string
--- @param vec vec3
function shaderFunctions:sendVec3(name, vec)
    tempTableVec3[1] = vec.x
    tempTableVec3[2] = vec.y
    tempTableVec3[3] = vec.z

    self.shader:send(name, tempTableVec3)
end

local tempTableVec4 = { 0, 0, 0, 0 }

--- Sends a vec4 to the shader
--- @param name string
--- @param vec vec4
function shaderFunctions:sendVec4(name, vec)
    tempTableVec4[1] = vec.x
    tempTableVec4[2] = vec.y
    tempTableVec4[3] = vec.z
    tempTableVec4[4] = vec.w

    self.shader:send(name, tempTableVec4)
end

--- Sends a quaternion to the shader
--- @param name string
--- @param quat quaternion
function shaderFunctions:sendQuat(name, quat)
    tempTableVec4[1] = quat.x
    tempTableVec4[2] = quat.y
    tempTableVec4[3] = quat.z
    tempTableVec4[4] = quat.w

    self.shader:send(name, tempTableVec4)
end

function shaderFunctions:getDebugName()
    return self.name
end

function shaderFunctions:hasUniform(name)
    if self.uniforms[name] == nil then
        self.uniforms[name] = self.shader:hasUniform(name)
    end

    return self.uniforms[name]
end

function shaderFunctions:getLocalThreadgroupSize()
    return self.shader:getLocalThreadgroupSize()
end

function shaderFunctions:use()
    love.graphics.setShader(self.shader)
end

--- add a function to be called when the shader is reloaded
---@param func fun(shader:Rhodium.shader)
function shaderFunctions:onReload(func)
    table.insert(self.onReloadFunctions, func)
end

--- Sets the current shader to the one provided
---@param shader? Rhodium.shader
function Rhodium.graphics.setShader(shader)
    love.graphics.setShader(shader and shader.shader)
end

--- Dispatches the compute shader
--- @param shader Rhodium.shader
--- @param width integer
--- @param height integer
--- @param depth integer
function Rhodium.graphics.dispatchThreadgroups(shader, width, height, depth)
    love.graphics.dispatchThreadgroups(shader.shader, width, height, depth)
end
