---
--- Neovide Custom Functions
---

vim.api.nvim_create_user_command("NeovideToggleFullscreen",
    "let g:neovide_fullscreen = !g:neovide_fullscreen", {})
vim.keymap.set("n", "<F11>", "<cmd>NeovideToggleFullscreen<CR>", { desc = "[Neovide] Toggle Fullscreen" })
