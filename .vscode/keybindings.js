[
    // Existing keybindings you provided
    {
        "key": "ctrl+k ctrl+q",
        "command": "-workbench.action.navigateToLastEditLocation"
    },
    {
        "key": "ctrl+q",
        "command": "-extension.vim_winCtrlQ",
        "when": "editorTextFocus && vim.active && vim.use<C-q> && !inDebugRepl"
    },
    {
        "key": "ctrl+q",
        "command": "-workbench.action.quickOpenNavigateNextInViewPicker",
        "when": "inQuickOpen && inViewsPicker"
    },
    {
        "key": "ctrl+shift+q",
        "command": "-workbench.action.quickOpenNavigatePreviousInViewPicker",
        "when": "inQuickOpen && inViewsPicker"
    },
    {
        "key": "ctrl+q",
        "command": "-workbench.action.quickOpenView"
    },
    {
        "key": "ctrl+e h",
        "command": "workbench.action.tasks.runTask",
        "args": "Export Org to HTML"
    },
    // Unbind default commands
    {
        "key": "ctrl+h",
        "command": "-editor.action.startFindReplaceAction"
    },
    {
        "key": "ctrl+k",
        "command": "-editor.action.deleteLines",
        "when": "editorTextFocus && !editorReadonly"
    },
    // Set keys to do nothing globally
    {
        "key": "ctrl+h",
        "command": ""
    },
    {
        "key": "ctrl+k",
        "command": ""
    },
    {
        "key": "ctrl+j",
        "command": ""
    },
    {
        "key": "ctrl+l",
        "command": ""
    },
    // Add navigation keybindings for Vim normal mode
    {
        "key": "ctrl+j",
        "command": "workbench.action.focusBelowGroup",
        "when": "editorTextFocus && vim.active && vim.mode == 'Normal'"
    },
    {
        "key": "ctrl+k",
        "command": "workbench.action.focusAboveGroup",
        "when": "editorTextFocus && vim.active && vim.mode == 'Normal'"
    },
    {
        "key": "ctrl+h",
        "command": "workbench.action.focusLeftGroup",
        "when": "editorTextFocus && vim.active && vim.mode == 'Normal'"
    },
    {
        "key": "ctrl+l",
        "command": "workbench.action.focusRightGroup",
        "when": "editorTextFocus && vim.active && vim.mode == 'Normal'"
    }
]
