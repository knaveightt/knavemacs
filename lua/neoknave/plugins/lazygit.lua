return {
    "kdheepak/lazygit.nvim",
    dependencies = {
        "nvim-lua/plenary.nvim"
    },
    config = function()
        local keymap = vim.keymap

        keymap.set("n", "<leader>pg", "<cmd>LazyGit<CR>", { desc="[LazyGit] Launch LazyGit" } )
    end
}
