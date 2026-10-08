{
  config,
  lib,
  pkgs,
  ...
}:

let
  workingDirectory = "/var/lib/linux-builder";

  package = pkgs.darwin.linux-builder-vz.override {
    modules = [
      {
        virtualisation.vz.nestedVirtualization = true;
        virtualisation.vz.rosetta.enable = false;
        virtualisation.cores = 8;
        virtualisation.darwin-builder.memorySize = 16 * 1024;
        virtualisation.darwin-builder.diskSize = 60 * 1024;
      }
    ];
  };

  hostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=";

  features = lib.concatStringsSep "," [
    "kvm"
    "nixos-test"
    "big-parallel"
    "benchmark"
  ];
in
{
  system.activationScripts.preActivation.text = ''
    mkdir -p ${workingDirectory}
  '';

  launchd.daemons.linux-builder = {
    environment = {
      inherit (config.environment.variables) NIX_SSL_CERT_FILE;
    };

    script = ''
      export TMPDIR=/run/org.nixos.linux-builder USE_TMPDIR=1
      rm -rf $TMPDIR
      mkdir -p $TMPDIR
      trap "rm -rf $TMPDIR" EXIT
      ${package}/bin/create-builder
    '';

    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
      WorkingDirectory = workingDirectory;
    };
  };

  environment.etc."ssh/ssh_config.d/100-linux-builder.conf".text = ''
    Host linux-builder
      User builder
      Hostname localhost
      HostKeyAlias linux-builder
      Port 31022
      IdentityFile /etc/nix/builder_ed25519
  '';

  determinateNix.customSettings = {
    builders = "ssh-ng://builder@linux-builder aarch64-linux /etc/nix/builder_ed25519 2 1 ${features} - ${hostKey}";
    builders-use-substitutes = true;
  };
}
