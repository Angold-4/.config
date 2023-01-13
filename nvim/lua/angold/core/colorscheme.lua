-- change the background color to totally black
vim.api.nvim_command([[
    augroup ChangeBackgroudColour
        autocmd colorscheme * :hi normal guibg=#000000
    augroup END
]])

vim.o.termguicolors = true

local status, _ = pcall(vim.cmd, "silent! colorscheme kanagawa")

if not status then
  print("colorscheme not found! please install it first!")
  return
end
