-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.font = wezterm.font("Cascadia Code PL", {weight="Regular", stretch="Normal", style="Normal"})
config.font_size = 14.0


-- Colorscheme
config.color_scheme = 'Tokyo Night'
config.colors = {
  background = 'black',
}

-- Tab
config.enable_tab_bar = false
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
-- Top bar
config.window_decorations = "RESIZE"


-- and finally, return the configuration to wezterm
return config
