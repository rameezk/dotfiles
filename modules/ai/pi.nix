{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.ai.pi;
in
{
  options.ai.pi = {
    enable = lib.mkEnableOption "pi coding agent (pi.dev)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.pi-coding-agent ];
  };
}
