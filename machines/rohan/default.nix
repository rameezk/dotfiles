{ home-manager, emacs-overlay }@inputs: {
  rohan = home-manager.lib.homeManagerConfiguration {
    configuration = { ... }: {
      nixpkgs.overlays = [ emacs-overlay.overlay ];
      imports = [ ./home.nix ];
    };
    system = "x86_64-linux";
    homeDirectory = "/home/rameezk";
    username = "rameezk";
  };
}
