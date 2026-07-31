{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.theme.catppuccin;

  configDir = "${config.home.homeDirectory}/.config";

  themeName = flavour: "Catppuccin ${lib.toSentenceCase flavour}";

  deltaInclude = flavour: isLight: ''
    [delta]
        syntax-theme = "${themeName flavour}"
        light = ${lib.boolToString isLight}
  '';

  batConfig =
    flavour:
    lib.concatMapStrings (line: "${line}\n") (
      lib.mapAttrsToList (n: v: "--${n}='${toString v}'") (
        config.programs.bat.config // { theme = themeName flavour; }
      )
    );

  themeSync = pkgs.writeShellApplication {
    name = "theme-sync";
    runtimeInputs = [ ];
    text = ''
      config_dir="''${XDG_CONFIG_HOME:-$HOME/.config}"

      flavour="${cfg.flavour}"
      if [ "$(uname)" = "Darwin" ] && command -v defaults >/dev/null 2>&1; then
        if ! defaults read -g AppleInterfaceStyle >/dev/null 2>&1; then
          flavour="${cfg.lightFlavour}"
        fi
      fi

      if [ "$flavour" = "${cfg.lightFlavour}" ]; then
        starship_target="$config_dir/starship-light.toml"
      else
        starship_target="$config_dir/starship.toml"
      fi

      active="$config_dir/starship-active.toml"
      current="$(readlink "$active" 2>/dev/null || true)"

      if [ "$current" = "$starship_target" ] && [ "''${1:-}" != "--force" ]; then
        exit 0
      fi

      if [ -n "''${TMUX:-}" ] && command -v tmux >/dev/null 2>&1; then
        tmux show -g 2>/dev/null | sed -nE 's/^(@(thm|catppuccin|_ctp)[^ ]*).*/\1/p' | while read -r opt; do
          tmux set -gu "$opt" 2>/dev/null || true
        done
        tmux source-file "$config_dir/tmux/tmux.conf" >/dev/null 2>&1 || true
      fi

      if [ "$flavour" = "${cfg.lightFlavour}" ]; then
        delta_target="$config_dir/git/delta-light.inc"
        bat_target="$config_dir/bat-light.conf"
        hunk_target="$config_dir/hunk/config-light.toml"
      else
        delta_target="$config_dir/git/delta-dark.inc"
        bat_target="$config_dir/bat/config"
        hunk_target="$config_dir/hunk/config-dark.toml"
      fi

      mkdir -p "$config_dir/git"
      ln -sfn "$delta_target" "$config_dir/git/delta-active.inc"
      ln -sfn "$bat_target" "$config_dir/bat-active.conf"
      ${lib.optionalString config.vcs.hunk.enable ''
        mkdir -p "$config_dir/hunk"
        ln -sfn "$hunk_target" "$config_dir/hunk/config.toml"
      ''}
      ln -sfn "${config.catppuccin.sources.fzf}/catppuccin-fzf-$flavour.rc" "$config_dir/fzf-active.rc"
      ln -sfn "$starship_target" "$active"
    '';
  };
in
{
  options.theme.catppuccin = {
    enable = lib.mkEnableOption "enable catppuccin";

    flavour = lib.mkOption {
      type = lib.types.str;
      default = "frappe";
      description = "Catppuccin flavour used when the OS appearance is dark.";
    };

    lightFlavour = lib.mkOption {
      type = lib.types.str;
      default = "latte";
      description = "Catppuccin flavour used when the OS appearance is light.";
    };

    followAppearance = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Follow the macOS light/dark appearance setting at runtime.";
    };

    syncPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      default = themeSync;
      description = "Script that repoints theme symlinks and rethemes tmux.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (!cfg.enable) {
      catppuccin = {
        enable = false;
        autoEnable = false;
      };
    })

    (lib.mkIf cfg.enable (
      lib.mkMerge [
        {
          catppuccin = {
            enable = true;
            autoEnable = true;
            flavor = cfg.flavour;

            tmux = {
              enable = false;
              extraConfig = ''
                set -g @catppuccin_window_right_separator "█ "
                set -g @catppuccin_window_number_position "right"
                set -g @catppuccin_window_middle_separator " | "

                set -g @catppuccin_window_default_fill "none"

                set -g @catppuccin_window_current_fill "all"

                set -g @catppuccin_status_modules_right "date_time session"

                set -g @catppuccin_status_left_separator "█"
                set -g @catppuccin_status_right_separator "█"

                set -g @catppuccin_date_time_text "%a %d/%m %H:%M"
              '';
            };
          };

          theme.active = {
            enable = true;
            darkFlavour = cfg.flavour;
            lightFlavour = cfg.lightFlavour;
            followAppearance = cfg.followAppearance;
            syncPackage = themeSync;

            weztermScheme = {
              dark = themeName cfg.flavour;
              light = themeName cfg.lightFlavour;
            };

            starshipLightSettings = {
              palette = "catppuccin_${cfg.lightFlavour}";
            }
            // lib.importTOML "${config.catppuccin.sources.starship}/${cfg.lightFlavour}.toml";

            tmuxConfig = ''
              if-shell 'command -v defaults >/dev/null 2>&1 && ! defaults read -g AppleInterfaceStyle >/dev/null 2>&1' \
                'set -g @catppuccin_flavor "${cfg.lightFlavour}"' \
                'set -g @catppuccin_flavor "${cfg.flavour}"'

              set -g @catppuccin_window_status_style "rounded"
              set -g @catppuccin_window_default_text " #W"
              set -g @catppuccin_window_current_text " #W#{?window_zoomed_flag,(),}"
              set -g @catppuccin_window_text " #W"
              run ~/.config/tmux/plugins/catppuccin/tmux/catppuccin.tmux

              set -gF message-style "fg=#{@thm_teal},bg=#{@thm_mantle},fill=#{@thm_mantle}"
              set -gF message-command-style "fg=#{@thm_teal},bg=#{@thm_mantle},fill=#{@thm_mantle}"

              set -g status-right-length 100
              set -g status-left-length 100
              set -g status-left ""
              set -g status-right "#{E:@catppuccin_status_application}"
              set -ag status-right "#{E:@catppuccin_status_session}"
              set -ag status-right "#{E:@catppuccin_status_uptime}"
              ${lib.optionalString cfg.followAppearance ''set -ag status-right "#(${themeSync}/bin/theme-sync)"''}
            '';
          };
        }

        (lib.mkIf config.editor.neovim.enable {
          programs.nixvim.colorschemes.catppuccin = {
            enable = true;
            settings = {
              transparent_background = true;
              flavour = "auto";
              background = {
                light = cfg.lightFlavour;
                dark = cfg.flavour;
              };
              integrations = {
                cmp = true;
                gitsigns = true;
                treesitter = true;
              };
            };
          };
        })

        (lib.mkIf config.vcs.hunk.enable {
          vcs.hunk = {
            darkTheme = "catppuccin-${cfg.flavour}";
            lightTheme = "catppuccin-${cfg.lightFlavour}";
          };
        })

        (lib.mkIf cfg.followAppearance {
          home.packages = [ themeSync ];

          home.sessionVariables = {
            STARSHIP_CONFIG = lib.mkForce "${configDir}/starship-active.toml";
            FZF_DEFAULT_OPTS_FILE = lib.mkForce "${configDir}/fzf-active.rc";
            BAT_CONFIG_PATH = "${configDir}/bat-active.conf";
          };

          xdg.configFile = {
            "git/delta-dark.inc".text = deltaInclude cfg.flavour false;
            "git/delta-light.inc".text = deltaInclude cfg.lightFlavour true;
            "bat-light.conf".text = batConfig cfg.lightFlavour;
          };

          home.activation.themeSync = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            run ${themeSync}/bin/theme-sync || true
          '';
        })
      ]
    ))
  ];
}
