return {
    "EdenEast/nightfox.nvim",
    name = "nightfox",
    lazy = false,
    priority = 1000, -- load before all others
    config = function()
        -- loading colorscheme here
        vim.cmd([[colorscheme nordfox]])
    end,
}
