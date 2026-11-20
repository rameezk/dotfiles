{
  determinateNix = {
    distributedBuilds = true;
    buildMachines = [
      {
        protocol = "ssh-ng";
        sshUser = "nix-ssh";
        hostName = "edoras";
        systems = [ "x86_64-linux" ];
        sshKey = "/etc/nix/edoras_builder_ed25519";
        maxJobs = 1;
        speedFactor = 1;
        supportedFeatures = [
          "kvm"
          "nixos-test"
          "big-parallel"
          "benchmark"
        ];
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSU93enV3Z3VINllmUmxNS0FOa2gwZFNWVE1tYkNpT0NNeU1mMllXT2FPUEggcm9vdEBlZG9yYXMK";
      }
    ];
    customSettings.builders-use-substitutes = true;
  };
}
