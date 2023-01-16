local status, ts = pcall(require, 'nvim-treesitter.configs')

if (not status) then return end;

ts.setup {
  highlight = {
    enable = true;
    disable = {},
  },
  indent = {
    enable = true,
    disable = {},
  },

  ensure_installed = {
    'c',
    'cpp',
    'bash',
    'go',
    'java',
    'javascript',
    'json',
    'lua',
    'make',
    'python',
    'gitcommit',
    'html',
    'markdown',
    'rust',
    'sql',
    'css',
  },

  autotag = {
    enable = true,
  }

}
