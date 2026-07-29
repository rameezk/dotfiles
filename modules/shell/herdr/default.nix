{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.herdr;
in
{

  options.herdr = {
    enable = lib.mkEnableOption "enable herdr";

    configPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.config/dotfiles/modules/shell/herdr/config.toml";
      description = "Absolute path to the live config.toml symlinked into ~/.config/herdr.";
    };
  };

  config = lib.mkIf cfg.enable {

    verify.checks = [
      {
        type = "command";
        name = "herdr";
        desc = "Agent multiplexer";
      }
      {
        type = "file";
        path = "~/.config/herdr/config.toml";
        desc = "herdr config";
      }
    ];

    home.packages = with pkgs; [
      herdr
    ];

    xdg.configFile."herdr/config.toml".source = config.lib.file.mkOutOfStoreSymlink cfg.configPath;
  };
}
