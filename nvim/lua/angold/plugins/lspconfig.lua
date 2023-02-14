local status, nvim_lsp = pcall(require, 'lspconfig')
if (not status) then return end

local protocol = require('vim.lsp.protocol')

local on_attach = function(client, bufnr)
  -- formatting
  if client.server_capabilities.documentFormattingProvider then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer lua vim.lsp.buf.formatting_seq_sync()]]
    vim.api.nvim_command [[augroup END]]
  end
end

vim.diagnostic.config({virtual_text = false, signs = false})

nvim_lsp.ccls.setup {
  init_options = {
    cache = {
      directory = ".ccls-cache";
    };
  }
}

nvim_lsp.pyright.setup{}

-- nvim_lsp.ccls.setup {
--   init_options = {
--     on_attach = on_attach,
--     compilationDatabaseDirectory = "build",
--     index = {
--       threads = 0;
--     };
--     clang = {
--       excludeArgs = { "-frounding-math"} ;
--     };
--   }
-- }

nvim_lsp.sumneko_lua.setup {
  on_attach = on_attach,
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the 'vim' global
        globals = {'vim'}
      },

      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true)
      }
    }
  }
}

require'lspconfig'.eslint.setup{}
