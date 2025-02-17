local opt = vim.opt -- for conciseness

-- line numbers
opt.relativenumber = true
opt.number = true

-- tabs & indentation
opt.showtabline = 2
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.autoindent = true

-- line wrapping
opt.wrap = false

-- search settings
opt.ignorecase = true
opt.smartcase = true

-- appearance
opt.termguicolors = true
opt.background = "dark"
opt.signcolumn = "yes"

-- backspace
opt.backspace = "indent,eol,start"

-- clipboard
opt.clipboard:append("unnamedplus")

-- split windows
opt.splitright = true
opt.splitbelow = true

opt.iskeyword:append("-")

-- encoding
opt.encoding = 'utf-8'
opt.fileencoding = 'utf-8'

vim.cmd('set encoding=utf-8')
vim.cmd('set fileencoding=utf-8')

vim.g.copilot_enabled = false -- copilot is disabled by default
-- could be enabled by ":Copilot enable"
