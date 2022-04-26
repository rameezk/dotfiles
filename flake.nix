{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    declarative-cachix.url = "github:jonascarpay/declarative-cachix/master";
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, declarative-cachix
    , ... }@inputs:
    let
      mkMachines = { }: {
        rohan = home-manager.lib.homeManagerConfiguration {
          configuration = { ... }: {
            nixpkgs.overlays = [ emacs-overlay.overlay ];
            imports = [ ./machines/rohan/home.nix ];
          };
          system = "x86_64-linux";
          homeDirectory = "/home/rameezk";
          username = "rameezk";
        };
        rivendell = home-manager.lib.homeManagerConfiguration {
          configuration = { ... }: {
            nixpkgs.overlays = [ emacs-overlay.overlay ];
            imports = [
              declarative-cachix.homeManagerModules.declarative-cachix
              ./machines/rivendell/home.nix
            ];
          };
          system = "x86_64-darwin";
          homeDirectory = "/Users/rameezk";
          username = "rameezk";
        };
        gondor = home-manager.lib.homeManagerConfiguration {
          configuration = { ... }: {
            nixpkgs.overlays = [ emacs-overlay.overlay ];
            imports = [
              declarative-cachix.homeManagerModules.declarative-cachix
              ./machines/gondor/home.nix
            ];
          };
          system = "aarch64-darwin";
          homeDirectory = "/Users/qxy6675";
          username = "qxy6675";
        };
      };
    in {
      machines = mkMachines { };

      rohan = self.machines.rohan.activationPackage;
      rivendell = self.machines.rivendell.activationPackage;
    };
}
