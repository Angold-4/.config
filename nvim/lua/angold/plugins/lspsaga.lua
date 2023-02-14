local status, saga = pcall(require, 'lspsaga')
if (not status) then return end

saga.setup({
  -- keybinds for navigation in lspsaga window
  scroll_preview = { scroll_down = "<C-j>", scroll_up = "<C-k>" },
  -- use enter to open file with definition preview
  definition = {
    edit = "<CR>",
  },
  ui = {
    theme = 'round',
    colors = {
      normal_bg = "#16161D",
    },
    winblend = 0,
  },
  symbol_in_winbar = {
    enable = false,
  }
})

local opts = {noremap = true, silent = true}

vim.keymap.set('n', '<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<cr>', opts)
vim.keymap.set('n', '<C-k>', '<Cmd>Lspsaga diagnostic_jump_prev<cr>', opts)
vim.keymap.set('n', 'K', '<Cmd>Lspsaga hover_doc<cr>', opts)
vim.keymap.set('n', 'gf', '<Cmd>Lspsaga lsp_finder<cr>', opts)
vim.keymap.set('i', '<C-h>', '<Cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
vim.keymap.set('n', 'rn', '<Cmd>Lspsaga rename<cr>', opts)
vim.keymap.set('n', '<leader>d', '<Cmd>Lspsaga show_line_diagnostics<cr>', opts)
vim.keymap.set('n', '<leader>d', '<Cmd>Lspsaga show_cursor_diagnostics<cr>', opts)
