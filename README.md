# mobdap

Experimental implementation of the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) for [MobDebug](https://github.com/pkulchenko/MobDebug)

## How it works

This application implements everything thats necessary for communicating with your editor using DAP via stdio and re-implements
all the MobDebug server functionality, waiting for a client to hook into the server

## Configuration

### Your project

Download a copy of [mobdebug.lua](https://github.com/pkulchenko/MobDebug/blob/master/src/mobdebug.lua) and put it somewhere into your
project.

When you want to debug import it and call the start function:

```lua
-- perhaps hidden behind an env variable or whatever
if enable_debugger then
    local debugger = require "mobdebug"
    debugger.start("127.0.0.1", 18172) -- NOTE: the port here, this has to be the same as in your editor config
end
```

### Editor

#### Neovim

```lua
local dap = require "dap"

dap.adapters.mobdap = {
    id = "mobdap",
    type = "executable",
    command = "/path/to/mobdap",
}

dap.configurations.lua = {
    {
        name = "MobDebug",
        type = "mobdap",
        request = "launch",

        -- these are the parameters you might want to configure

        -- rootdir: The path to your project root
        rootdir = function()
            return vim.fs.root(0, { ".git" })
        end,

        -- sourcedirs: (Optional) Alternative search dirs for dependencies
        sourcedirs = {
            "/home/USERNAME/.luarocks/share/lua/5.1",
        },

        -- port: (Optional: Defaults to 18172) the port on which the server is running
        --       Please configure the same on your client
        port = 18172,
    },
}
```

## See also

- Using mobdap with **Defold**: [defold-mobdebug](https://github.com/atomicptr/defold-mobdebug)
- Debugger integrated with Neovim for Defold: [defold.nvim](https://github.com/atomicptr/defold.nvim)

## License

GPLv3
