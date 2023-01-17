local status, _ = pcall(require, 'Navigator')
if (not status) then return end


vim.keymap.set({'n', 't'}, '<C-l>', '<CMD>NavigatorLeft<CR>')
vim.keymap.set({'n', 't'}, '<C-h', '<CMD>NavigatorRight<CR>')
vim.keymap.set({'n', 't'}, '<A-k>', '<CMD>NavigatorUp<CR>')
vim.keymap.set({'n', 't'}, '<A-j>', '<CMD>NavigatorDown<CR>')
