return {
    "nvim-telescope/telescope.nvim", tag = "0.1.5",
    dependencies = {
        "nvim-lua/plenary.nvim"
    },
    config = function()
        -- configure telescope
        local telescope = require("telescope")
        local actions = require("telescope.actions")
        
        telescope.setup({
            defaults = {
                mappings = {
                    i = {
                        ["<C-k>"] = actions.move_selection_previous,
                        ["<C-j>"] = actions.move_selection_next,
                    }
                }
            }
        })

        -- keymaps
        local keymap = vim.keymap
        local builtin = require("telescope.builtin")
        
        keymap.set("n", "<leader>ff", builtin.find_files, { desc="[Telescope] Find Files" })
        keymap.set("n", "<leader>fg", builtin.grep_string, { desc="[Telescope] Grep String" })
        keymap.set("n", "<leader>bb", builtin.buffers, { desc="[Telescope] Show Buffers" })
        keymap.set("n", "<leader>bs", builtin.current_buffer_fuzzy_find, { desc="[Telescope] Search in Buffer" })
        keymap.set("n", "<leader>pf", builtin.git_files, { desc="[Telescope] Find Git Files" })
        keymap.set("n", "<leader>mm", builtin.marks, { desc="[Telescope] Show Marks" })
    end,
}
