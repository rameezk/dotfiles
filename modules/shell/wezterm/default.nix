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

        local function scheme_for_appearance(appearance)
          if appearance:find 'Dark' then
            return '${config.theme.active.weztermScheme.dark}'
          else
            return '${config.theme.active.weztermScheme.light}'
          end
        end

        local function current_appearance()
          if wezterm.gui then
            return wezterm.gui.get_appearance()
          end
          return 'Dark'
        end

        config.color_scheme = scheme_for_appearance(current_appearance())

        wezterm.on('window-config-reloaded', function(window)
          local overrides = window:get_config_overrides() or {}
          local scheme = scheme_for_appearance(window:get_appearance())
          if overrides.color_scheme ~= scheme then
            overrides.color_scheme = scheme
            window:set_config_overrides(overrides)
          end
        end)

        ${lib.optionalString
          (config.theme.active.followAppearance && config.theme.active.syncPackage != null)
          ''
            wezterm.on('window-config-reloaded', function()
              wezterm.background_child_process {
                '${config.theme.active.syncPackage}/bin/theme-sync',
              }
            end)
          ''
        }

        config.font = wezterm.font_with_fallback { 
          'MesloLGS NF',
          'JetBrains Mono'
        }
        config.font_size = 16

        config.window_background_opacity = 0.9
        config.macos_window_background_blur = 30

        config.native_macos_fullscreen_mode = true

        config.bypass_mouse_reporting_modifiers = 'CMD'
        config.keys = {
          {
            key = 'Enter',
            mods = 'CMD',
            action = wezterm.action.ToggleFullScreen,
          },
        }

        config.hide_tab_bar_if_only_one_tab = true

        return config
      '';

}
