function newTextureGroup(textures, settings)
    settings = settings or {}

    local width, height = 0, 0
    for i = 1, #textures do
        local texture = textures[i]

        local w, h = texture:getDimensions()

        width = math.max(width, w)
        height = math.max(height, h)
    end

    local scales = {}

    for i = 1, #textures do
        local texture = textures[i]

        local w, h = texture:getDimensions()

        scales[i] = { w / width, h / height }
    end

    local bufferFormat = {
        { name = "scale", format = "floatvec2" }
    }
    local buffer = newBuffer(bufferFormat, #textures, { shaderstorage = true, usage = "static" })
    buffer:write(scales):flush()
    ---@type love.Texture
    local texture = love.graphics.newArrayTexture(width, height, #textures,
        { format = settings.format, canvas = true, mipmaps = settings.mipmaps })
    texture:setWrap("repeat", "repeat")
    texture:setFilter("linear", "linear")

    love.graphics.push("all")
    love.graphics.setBlendMode("none")
    for i = 1, #textures do
        love.graphics.setCanvas(texture, i, 1)
        love.graphics.draw(textures[i])
    end

    love.graphics.pop()

    if settings.mipmaps ~= "none" and settings.mipmaps ~= nil then
        texture:generateMipmaps()
    end

    return texture, buffer
end
