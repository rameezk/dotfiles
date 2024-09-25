{ lib, config, ... }:

let
  cfg = config.terminal.wezterm;
in
{

  options.terminal.wezterm = {
    enable = lib.mkEnableOption "enable wezterm";
  };

  config.xdg.configFile."wezterm/wezterm.lua".text =
    lib.mkIf cfg.enable # lua
      ''
        local wezterm = require 'wezterm'
        local config = wezterm.config_builder()

        config.color_scheme = 'Catppuccin Frappe'

        config.font = wezterm.font 'JetBrains Mono'
        config.font_size = 16;

        config.hide_tab_bar_if_only_one_tab = true

        local dimmer = { brightness = 0.03 }
        config.background = {
          {
            source = {File = '${../../../wallpapers/forest.jpg}'},
            hsb = dimmer
          }
        }

        return config
      '';

}
