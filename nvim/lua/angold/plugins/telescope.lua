-- import telescope plugin safely
local telescope_setup, telescope = pcall(require, "telescope")
if not telescope_setup then
  return
end

-- import telescope actions safely
local actions_setup, actions = pcall(require, "telescope.actions")
if not actions_setup then
  return
end

local function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local fb_actions = require 'telescope'.extensions.file_browser.actions

local actions = require("telescope.actions")
-- local trouble = require("trouble.providers.telescope")


-- configure telescope
telescope.setup({
  -- configure custom mappings
  defaults = {
    mappings = {
      n = { 
        ['q'] = actions.close,
      },
      i = {
        ["<C-k>"] = actions.move_selection_previous, -- move to prev result
        ["<C-j>"] = actions.move_selection_next, -- move to next result
      },
    },
    extensions = {
      file_browser = {
        theme = 'dropdown',
        hijack_netrw = true,
      }
    }
  },
})

telescope.load_extension('file_browser');
local opts = { noremap = true, silent = true }
vim.keymap.set('n', ';f', '<cmd>lua require("telescope.buitin").find_files({ no_ignore = false, hidden = true })<cr>', opts)
telescope.load_extension("fzf")
