{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "/nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay/master";

    declarative-cachix.url = "github:jonascarpay/declarative-cachix/master";
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, emacs-overlay, declarative-cachix
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

      configuration = {pkgs, ... }: {
        environment.systemPackages = [ pkgs.neovim ];

        # Auto upgrade nix package and the daemon service.
        services.nix-daemon.enable = true;
        # nix.package = pkgs.nix;

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";

        users.users.rameezk.home = "/Users/rameezk";

        # Create /etc/zshrc that loads the nix-darwin environment.
        # programs.zsh.enable = true;  # default shell on catalina
        programs.fish.enable = true;

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        security.pam.enableSudoTouchIdAuth = true;
        system.defaults = {
          dock.autohide = true;
        };

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 4;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "aarch64-darwin";
      };
    in {
      machines = mkMachines { };

      rohan = self.machines.rohan.activationPackage;
      gondor = self.machines.gondor.activationPackage;

      darwinConfigurations."Rameezs-MacBook-Air" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [ 
          configuration
          home-manager.darwinModules.home-manager {
            home-manager.users.rameezk = import ./machines/rivendell/home.nix;
          }
        ];
      };
      darwinPackages = self.darwinConfigurations."Rameezs-MacBook-Air".pkgs;
    };
}
