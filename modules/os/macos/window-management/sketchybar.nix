{ lib, config, ... }:
let
  cfg = config.macos.window-management.sketchybar;
in
{
  options.macos.window-management.sketchybar = {
    enable = lib.mkEnableOption "enable sketchybar";
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."sketchybar".source = config.lib.file.mkOutOfStoreSymlink ./config/sketchybar;
  };
}
