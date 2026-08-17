{ lib, config, ... }:
let
  cfg = config.macos.karabiner;
in
{
  options.macos.karabiner = {
    enable = lib.mkEnableOption "link dotfiles Karabiner-Elements config";
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."karabiner/karabiner.json" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/dotfiles/modules/os/macos/karabiner/config/karabiner.json";
      force = true;
    };
  };
}
