{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    ./kubernetes
  ];

  options = {
    container.enable = lib.mkEnableOption "enable container runtime";
    container.podman = lib.mkEnableOption "use podman as container runtime";
  };

  config = lib.mkIf config.container.enable {
    home.packages =
      with pkgs;
      lib.optionals config.container.podman [
        podman
        podman-compose
        (writeShellScriptBin "docker" ''exec ${podman}/bin/podman "$@"'')
        (writeShellScriptBin "docker-compose" ''exec ${podman-compose}/bin/podman-compose "$@"'')
      ];
  };
}
