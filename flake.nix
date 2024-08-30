{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay/master";

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    declarative-cachix.url = "github:jonascarpay/declarative-cachix/master";
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, emacs-overlay, nixvim
    , declarative-cachix, ... }@inputs:
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
          pkgs = import nixpkgs {
            system = "aarch64-darwin";
            overlays = [ emacs-overlay.overlay ];
          };
          modules = [
            declarative-cachix.homeManagerModules.declarative-cachix
            ./machines/rivendell/home.nix
            {
              home = {
                username = "rameezk";
                homeDirectory = "/Users/rameezk";
              };
            }
          ];
        };
        gondor = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "aarch64-darwin";
            overlays = [ emacs-overlay.overlay ];
          };
          modules = [
            declarative-cachix.homeManagerModules.declarative-cachix
            nixvim.homeManagerModules.nixvim
            ./machines/gondor/home.nix
            {
              home = {
                username = "qxy6675";
                homeDirectory = "/Users/qxy6675";
              };
            }
          ];
        };
      };
    in {
      machines = mkMachines { };

      rohan = self.machines.rohan.activationPackage;
      gondor = self.machines.gondor.activationPackage;

      darwinConfigurations."Rameezs-MacBook-Air" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          home-manager.darwinModules.home-manager
          { home-manager.users.rameezk = import ./machines/rivendell/home.nix; }
          ./machines/rivendell
        ];
      };
      darwinPackages = self.darwinConfigurations."Rameezs-MacBook-Air".pkgs;
    };
}
