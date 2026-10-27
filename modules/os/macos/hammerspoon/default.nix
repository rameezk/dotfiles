{ lib, config, ... }:
let
  cfg = config.macos.hammerspoon;
in
{
  options.macos.hammerspoon = {
    enable = lib.mkEnableOption "Hammerspoon config that pauses media while the microphone is in use";
  };

  config = lib.mkIf cfg.enable {
    home.file.".hammerspoon/init.lua".source = ./init.lua;
  };
}
