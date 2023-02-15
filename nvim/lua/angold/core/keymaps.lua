local keymap = vim.keymap -- for conciseness

-- general keymaps

-- 1. remapping the esc, feels right
keymap.set("i", "jk", "<ESC>")
keymap.set("i", "Jk", "<ESC>")
keymap.set("i", "JK", "<ESC>")
keymap.set("i", "jK", "<ESC>")

-- 2. remapping the leader key and some others
vim.g.mapleader = " "
keymap.set("n", "<leader>nh", ":nohl<CR>") -- no highlight
keymap.set("n", "<leader>+", "<C-a>") -- leader plus to increase number

keymap.set("n", "H", "^") -- upper 'H' move to the begin of the line
keymap.set("n", "L", "$") -- upper 'L' move to the end of the line

keymap.set('n', 'dw', 'vb"_d') -- delete a word backwards

keymap.set('n', '<C-a>', 'gg<S-v>G') -- select all

-- 3. split the windows
keymap.set("n", "<leader>sv", "<C-w>v") -- split vertically
keymap.set("n", "<leader>sh", "<C-w>s") -- split horizontally
keymap.set("n", "<leader>se", "<C-w>=") -- make split windows equal width
keymap.set("n", "<leader>sx", ":close<CR>") -- close current split window

-- 4. tab settings
-- keymap.set("n", "<C-t>", ":tabnew<CR>") -- open new tab
-- keymap.set("n", "<leader>tx", ":tabclose<CR>") -- close current tab
-- keymap.set("n", "<leader>tn", ":tabn<CR>") -- go to next tab
-- keymap.set("n", "<leader>tp", ":tabp<CR>") -- go to previous tab

-- 5. plugin keymaps
-- vim-maximizer
keymap.set("n", "<leader>sm", ":MaximizerToggle<CR>")

-- nvim-tree
keymap.set("n", "<C-n>", ":NvimTreeToggle<CR>")

-- telescope 
keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>") -- find files within current working directory, respects .gitignore
keymap.set("n", "<leader>fs", "<cmd>Telescope live_grep<cr>") -- find string in current working directory as you type
keymap.set("n", "<leader>fc", "<cmd>Telescope grep_string<cr>") -- find string under cursor in current working directory
keymap.set("n", "<leader>fb", "<cmd>Telescope buffers<cr>") -- list open buffers in current neovim instance
keymap.set("n", "<leader>fh", "<cmd>Telescope help_tags<cr>") -- list available help tags

-- cellular-automaton
keymap.set("n", "<leader>rain", "<cmd>CellularAutomaton make_it_rain<CR>")

-- tabtab
keymap.set("n", "<leader>k", "<Cmd>BufferPrevious<CR>")
keymap.set("n", "<leader>j", "<Cmd>BufferNext<CR>")
keymap.set("n", "<leader>x", "<Cmd>BufferClose<CR>")

-- toggleterm
keymap.set({"n", "t"}, "<C-]>", "<Cmd>lua _HTOP_TOGGLE()<CR>")

-- trouble
keymap.set('n', '<C-w>', '<Cmd>TroubleToggle<CR>')
