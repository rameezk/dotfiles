{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, ... }@inputs: {
    workbook = home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay ];

        imports = [ ./home.nix ];
      };

      system = "x86_64-linux";
      homeDirectory = "/home/rameezk";
      username = "rameezk";
    };

    rohan = self.workbook.activationPackage;
  };
}
