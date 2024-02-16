return {
    "williamboman/mason.nvim",
    dependencies = {
        "williamboman/mason-lspconfig.nvim"
    },
    config = function()
        -- import mason and mason_lspconfig
        local mason = require("mason")
        local mason_lspconfig = require("mason-lspconfig")

        -- enable mason and some configured icons
        mason.setup({
            ui = {
                icons = {
                    package_installed = "✓",
                    package_pending = "➜",
                    package_uninstalled = "✗",
                },
            },
        })


        -- lspconfig setup
        mason_lspconfig.setup({
            -- default servers to install
            ensure_installed = {},
            automatic_installation = true,
        })
    end
}
