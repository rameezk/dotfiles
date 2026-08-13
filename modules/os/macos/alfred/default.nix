{ lib, config, ... }:
let
  cfg = config.macos.alfred;
  installScript = "${config.home.homeDirectory}/.config/dotfiles/modules/os/macos/alfred/install.sh";
in
{
  options.macos.alfred = {
    enable = lib.mkEnableOption "link dotfiles Alfred workflows into Alfred";
  };

  config = lib.mkIf cfg.enable {
    home.activation.alfredWorkflows = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ -x "${installScript}" ]; then
        run "${installScript}"
      fi
    '';
  };
}
