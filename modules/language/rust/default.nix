{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    language.rust.enable = lib.mkEnableOption "enable rust";
  };

  config = lib.mkIf config.language.rust.enable {

    verify.checks = [
      {
        type = "command";
        name = "rustc";
        desc = "Rust compiler";
      }
      {
        type = "command";
        name = "cargo";
        desc = "Cargo package manager";
      }
      {
        type = "command";
        name = "rustfmt";
        desc = "Rust formatter";
      }
    ];

    home.packages = with pkgs; [
      rustc
      cargo
      rustfmt
    ];
  };
}
