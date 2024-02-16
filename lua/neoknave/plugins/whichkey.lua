return {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
        vim.o.timeout = true
        vim.o.timeoutlen = 300
    end,
    opts = {
    },
    config = function()
        local wk = require("which-key")
        wk.register({
            b = {
                name = "+Buffer Actions",
            },
            t = {
                name = "+Tabpage Actions",
            },
            f = {
                name = "+File Actions",
            },
            m = {
                name = "+Mark Actions",
            },
            p = {
                name = "+Project Actions",
            },
        }, { prefix="<leader>" })
    end,
}
