local status, nvim_lsp = pcall(require, 'lspconfig')
if (not status) then return end

vim.lsp.handlers["textDocument/codeAction"] = function(_, _, actions)
    if actions == nil or next(actions) == nil then
        return
    end
    vim.lsp.util.show_code_actions(actions)
end

local on_attach = function(client, bufnr)
  -- Formatting
  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()]]
    vim.api.nvim_command [[augroup END]]
  end

  -- Disable the lightbulb
  client.resolved_capabilities.code_action = false
end

vim.diagnostic.config({virtual_text = false, signs = true})

_G.diagnostic_win = nil

function _G.toggle_diagnostics()
  if _G.diagnostic_win and vim.api.nvim_win_is_valid(_G.diagnostic_win) then
    vim.api.nvim_win_close(_G.diagnostic_win, true)
    _G.diagnostic_win = nil
  else
    local current_win = vim.api.nvim_get_current_win()
    _G.diagnostic_win = vim.diagnostic.open_float({scope="line"})
    vim.api.nvim_set_current_win(current_win)
  end
end

vim.api.nvim_set_keymap('n', '<C-w>', '<cmd>lua _G.toggle_diagnostics()<CR>', {noremap = true, silent = true})

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

nvim_lsp.lua_ls.setup {
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

nvim_lsp.rust_analyzer.setup {
  -- rust-tools options
  tools = {
    autoSetHints = true,
    hover_with_actions = true,
    inlay_hints = {
      show_parameter_hints = true,
      parameter_hints_prefix = "",
      other_hints_prefix = "",
      },
    },

  -- all the opts to send to nvim-lspconfig
  -- these override the defaults set by rust-tools.nvim
  -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
  -- https://rust-analyzer.github.io/manual.html#features
  server = {
    settings = {
      ["rust-analyzer"] = {
        assist = {
          importEnforceGranularity = true,
          importPrefix = "crate"
          },
        cargo = {
          allFeatures = true
          },
        checkOnSave = {
          -- default: `cargo check`
          command = "clippy"
          },
        },
        inlayHints = {
          lifetimeElisionHints = {
            enable = true,
            useParameterNames = true
          },
        },
      }
    },
}

nvim_lsp.gopls.setup {
  on_attach = on_attach,
  autoSetHints = false,
  settings = {
    gopls = {
      analyses = {
        nilness = true,
        unusedparams = true,
        unusedwrite = true,
        useany = true,
      },
      experimentalPostfixCompletions = true,
      gofumpt = true,
      staticcheck = true,
      usePlaceholders = true,
    },
  },
}

require'lspconfig'.eslint.setup{}
