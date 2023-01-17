-- if packer not installed, then install it before open
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local status, packer = pcall(require, "packer")
if not status then -- if there is no packer, then we simply return
  return
end

vim.cmd [[packadd packer.nvim]]

return packer.startup(function(use)
  use {"wbthomason/packer.nvim"}
  use {"nvim-lua/plenary.nvim"}
  use {"rebelot/kanagawa.nvim"}
  use {"christoomey/vim-tmux-navigator"}
  use {"szw/vim-maximizer"}
  use {"numToStr/Comment.nvim"}

  use {"nvim-tree/nvim-tree.lua"}
  use {'nvim-tree/nvim-web-devicons'}

  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" }) -- dependency for better sorting performance
  use({ "nvim-telescope/telescope.nvim", branch = "0.1.x" }) -- fuzzy finder

  -- treesitter configuration
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }

  use('nvim-treesitter/playground')
  use('eandrju/cellular-automaton.nvim')
  use('theprimeagen/harpoon')

  use('glepnir/lspsaga.nvim')
  use('L3MON4D3/LuaSnip') -- Snippet
  use('hrsh7th/cmp-buffer') -- nvim-cmp source for buffer words
  use('hrsh7th/cmp-nvim-lsp') -- nvim-cmp source for neovim's built-in LSP
  use('hrsh7th/nvim-cmp') -- Completion

  use('neovim/nvim-lspconfig') -- LSP

  use('nvim-telescope/telescope-file-browser.nvim')

  use('norcalli/nvim-colorizer.lua')
  use('akinsho/nvim-bufferline.lua')

  if packer_bootstrap then
    require("packer").sync()
  end

end)

















