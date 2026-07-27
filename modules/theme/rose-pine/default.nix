{
  inputs,
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.theme.rose-pine;

  configDir = "${config.home.homeDirectory}/.config";

  palettes = import ./palette.nix;
  ezaTheme = import ./eza.nix;

  slug = flavour: if flavour == "main" then "rose-pine" else "rose-pine-${flavour}";

  paletteFor = flavour: palettes.${flavour};

  starshipPalette =
    flavour:
    let
      p = paletteFor flavour;
    in
    {
      black = p.overlay;
      red = p.love;
      green = p.pine;
      yellow = p.gold;
      blue = p.foam;
      purple = p.iris;
      cyan = p.rose;
      white = p.text;
      "bright-black" = p.muted;
      "bright-red" = p.love;
      "bright-green" = p.pine;
      "bright-yellow" = p.gold;
      "bright-blue" = p.foam;
      "bright-purple" = p.iris;
      "bright-cyan" = p.rose;
      "bright-white" = p.text;
      inherit (p)
        base
        surface
        overlay
        muted
        subtle
        text
        love
        gold
        rose
        pine
        foam
        iris
        ;
    };

  starshipSettings = flavour: {
    palette = slug flavour;
    palettes.${slug flavour} = starshipPalette flavour;
  };

  fzfThemes = pkgs.runCommand "rose-pine-fzf-rc" { } ''
    mkdir -p "$out"
    for f in ${inputs.rose-pine-fzf}/dist/*.sh; do
      grep -oE -- '--color=.*' "$f" | sed 's/"$//' > "$out/$(basename "$f" .sh).rc"
    done
  '';

  fishThemes = pkgs.runCommand "rose-pine-fish-themes" { } ''
    mkdir -p "$out"
    for f in ${inputs.rose-pine-fish}/themes/*.theme; do
      name="$(basename "$f" .theme)"
      case "$name" in
        *Auto*) continue ;;
        *Moon*) dest="rose-pine-moon" ;;
        *Dawn*) dest="rose-pine-dawn" ;;
        *) dest="rose-pine" ;;
      esac
      sed -E '/^[[:space:]]*#/d; /^[[:space:]]*$/d; s/^([^[:space:]]+)[[:space:]]*(.*)$/set -g \1 \2/' \
        "$f" > "$out/$dest.fish"
    done
  '';

  tmuxUptime = pkgs.writeShellScript "tmux-uptime" ''
    uptime | sed 's/^[^,]*up *//; s/, *[[:digit:]]* user.*//; s/ day.*, */d /; s/ hr\(s*\).*/h/; s/ min\(s*\).*/m/; s/ sec\(s*\).*/s/; s/\([0-9]\{1,2\}\):\([0-9]\{1,2\}\)/\1h \2m/;'
  '';

  tmuxUptimeSection =
    flavour:
    let
      p = paletteFor flavour;
    in
    " #[fg=${p.iris}]#(${tmuxUptime})#[fg=${p.subtle}]  #[fg=${p.subtle}] "
    + lib.optionalString cfg.followAppearance "#(${themeSync}/bin/theme-sync)";

  deltaInclude = flavour: isLight: ''
    [delta]
        syntax-theme = "${slug flavour}"
        light = ${lib.boolToString isLight}
  '';

  batConfig =
    flavour:
    lib.concatMapStrings (line: "${line}\n") (
      lib.mapAttrsToList (n: v: "--${n}='${toString v}'") (
        config.programs.bat.config // { theme = slug flavour; }
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
        tmux show -g 2>/dev/null | sed -nE 's/^(@rose_pine[^ ]*).*/\1/p' | while read -r opt; do
          tmux set -gu "$opt" 2>/dev/null || true
        done
        tmux source-file "$config_dir/tmux/tmux.conf" >/dev/null 2>&1 || true
      fi

      if [ "$flavour" = "${cfg.lightFlavour}" ]; then
        delta_target="$config_dir/git/delta-light.inc"
        bat_target="$config_dir/bat-light.conf"
        eza_target="$config_dir/eza/theme-light.yml"
      else
        delta_target="$config_dir/git/delta-dark.inc"
        bat_target="$config_dir/bat/config"
        eza_target="$config_dir/eza/theme-dark.yml"
      fi

      mkdir -p "$config_dir/git" "$config_dir/eza"
      ln -sfn "$delta_target" "$config_dir/git/delta-active.inc"
      ln -sfn "$bat_target" "$config_dir/bat-active.conf"
      ln -sfn "$eza_target" "$config_dir/eza/theme.yml"
      ln -sfn "${fzfThemes}/$(if [ "$flavour" = "main" ]; then echo rose-pine; else echo "rose-pine-$flavour"; fi).rc" "$config_dir/fzf-active.rc"
      ln -sfn "$starship_target" "$active"
    '';
  };
in
{
  options.theme.rose-pine = {
    enable = lib.mkEnableOption "enable rosé pine";

    flavour = lib.mkOption {
      type = lib.types.enum [
        "main"
        "moon"
        "dawn"
      ];
      default = "moon";
      description = "Rosé Pine variant used when the OS appearance is dark.";
    };

    lightFlavour = lib.mkOption {
      type = lib.types.enum [
        "main"
        "moon"
        "dawn"
      ];
      default = "dawn";
      description = "Rosé Pine variant used when the OS appearance is light.";
    };

    followAppearance = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Follow the macOS light/dark appearance setting at runtime.";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        theme.active = {
          enable = true;
          darkFlavour = cfg.flavour;
          lightFlavour = cfg.lightFlavour;
          followAppearance = cfg.followAppearance;
          syncPackage = themeSync;

          weztermScheme = {
            dark = slug cfg.flavour;
            light = slug cfg.lightFlavour;
          };

          starshipLightSettings = starshipSettings cfg.lightFlavour;

          tmuxConfig = ''
            if-shell 'command -v defaults >/dev/null 2>&1 && ! defaults read -g AppleInterfaceStyle >/dev/null 2>&1' \
              'set -g @rose_pine_variant "${cfg.lightFlavour}"' \
              'set -g @rose_pine_variant "${cfg.flavour}"'

            set -g @rose_pine_host "off"
            set -g @rose_pine_user "off"
            set -g @rose_pine_directory "off"
            set -g @rose_pine_date_time "%a %d/%m %H:%M"
            set -g @rose_pine_show_current_program "on"
            set -g @rose_pine_window_status_separator "  "
            set -g @rose_pine_disable_active_window_menu "on"

            if-shell 'command -v defaults >/dev/null 2>&1 && ! defaults read -g AppleInterfaceStyle >/dev/null 2>&1' \
              'set -g @rose_pine_status_right_append_section "${tmuxUptimeSection cfg.lightFlavour}"' \
              'set -g @rose_pine_status_right_append_section "${tmuxUptimeSection cfg.flavour}"'

            set -g status-right-length 100
            set -g status-left-length 100

            run-shell ${pkgs.tmuxPlugins.rose-pine}/share/tmux-plugins/rose-pine/rose-pine.tmux
          '';
        };

        programs.bat = {
          config.theme = slug cfg.flavour;
          themes = lib.listToAttrs (
            map
              (flavour: {
                name = slug flavour;
                value = {
                  src = "${inputs.rose-pine-tm-theme}/dist";
                  file = "${slug flavour}.tmTheme";
                };
              })
              [
                cfg.flavour
                cfg.lightFlavour
              ]
          );
        };

        programs.fish.interactiveShellInit = lib.mkAfter ''
          if defaults read -g AppleInterfaceStyle >/dev/null 2>&1
            source ${fishThemes}/${slug cfg.flavour}.fish
          else
            source ${fishThemes}/${slug cfg.lightFlavour}.fish
          end
        '';

        programs.starship.settings = starshipSettings cfg.flavour;
      }

      (lib.mkIf config.editor.neovim.enable {
        programs.nixvim.colorschemes.rose-pine = {
          enable = true;
          settings = {
            variant = "auto";
            dark_variant = cfg.flavour;
            styles.transparency = true;
          };
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
          "eza/theme-dark.yml".text = ezaTheme (paletteFor cfg.flavour);
          "eza/theme-light.yml".text = ezaTheme (paletteFor cfg.lightFlavour);
        };

        home.activation.themeSync = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          run ${themeSync}/bin/theme-sync || true
        '';
      })
    ]
  );
}
