local status, ts = pcall(require, 'nvim-treesitter.configs')

vim.api.nvim_exec([[
  autocmd BufRead,BufNewFile *.Rmd set filetype=markdown
]], false)

vim.api.nvim_exec([[
  autocmd BufRead,BufNewFile *.Rmarkdown set filetype=markdown
]], false)

if (not status) then return end;

ts.setup {
  highlight = {
    enable = true;
    disable = {},
    additional_vim_regex_highlighting = {'org'},
  },
  indent = {
    enable = true,
    disable = {},
  },

  ensure_installed = {
    'r',
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
