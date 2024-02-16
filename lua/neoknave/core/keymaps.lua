-- set leader key for keymaps, and way to escape
vim.g.mapleader=" "
local keymap = vim.keymap

--
-- buffer shortcuts
--
keymap.set("n", "<leader>bc", "<cmd>nohlsearch<CR>", { desc = "Clear Search Highlights" }) -- clear highlights
keymap.set("n", "<leader>bn", "<cmd>bnext<CR>", { desc = "Next Buffer" })
keymap.set("n", "<leader>bp", "<cmd>bprevious<CR>", { desc = "Prev Buffer" })
keymap.set("n", "<leader>bd", "<cmd>bd<CR>", { desc = "Close Buffer" })

--
-- tab shortcuts
--
keymap.set("n", "<leader>tn", "<cmd>tabnext<CR>", { desc = "Cycle Next Tab" })
keymap.set("n", "<leader>tp", "<cmd>tabprevious<CR>", { desc = "Cycle Prev Tab" })
keymap.set("n", "<leader>tc", "<cmd>tabclose<CR>", { desc = "Close Current Tab" })
keymap.set("n", "<leader>tt", "<cmd>tabnew<CR>", { desc = "Create New Tab" })

--
-- split management
--
keymap.set("n", "<leader>w", "<C-w>", { desc="+Window Cmd" })
keymap.set("n", "<C-h>", ":resize -1<CR>")
keymap.set("n", "<C-l>", ":resize +1<CR>")
keymap.set("v", "<C-h>", ":<BS><BS><BS><BS><BS>vertical resize -1<CR>v") -- make what split resized be mode determined 
keymap.set("v", "<C-l>", ":<BS><BS><BS><BS><BS>vertical resize +1<CR>v") -- (just a little trick), think v is vertical

--
-- line movements
--
keymap.set("n", "<C-j>", "ddp") -- single lines in normal mode
keymap.set("n", "<C-k>", "ddkP") -- single lines in normal mode
keymap.set("v", "<C-j>", "xp`[V`]")     -- multiple lines in visual mode
keymap.set("v", "<C-k>", "xkP`[V`]")      -- multiple lines in visual mode

--
-- marks management
--
keymap.set("n", "<leader>md", "<cmd>:delmarks A-Z<CR>")
keymap.set("n", "'", "`")

--
-- page scrolling
--
keymap.set("n", "<leader>q", "q", { desc="Record Macro" }) -- to clear use of q for the below
keymap.set("n", "q", "<C-u>")     -- 1/2 page scroll up
keymap.set("n", "a", "<C-d>")     -- 1/2 page scroll down
-- I can use li to have the same effect as 'a', or A at the end of a line
