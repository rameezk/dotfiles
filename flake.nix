{
    description = "Rameez's configuration";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

        nix-darwin = {
            url = "github:LnL7/nix-darwin";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        home-manager = {
            url = "github:rycee/home-manager/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        emacs-overlay.url = "github:nix-community/emacs-overlay/master";

        nixvim = {
            url = "github:nix-community/nixvim";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
        homebrew-bundle = {
            url = "github:homebrew/homebrew-bundle";
            flake = false;
        };
        homebrew-core = {
            url = "github:homebrew/homebrew-core";
            flake = false;
        };
        homebrew-cask = {
            url = "github:homebrew/homebrew-cask";
            flake = false;
        };

        sops-nix = {
            url = "github:Mic92/sops-nix";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        mysecrets = {
            url = "git+ssh://git@github.com/rameezk/nix-secrets.git?ref=main&shallow=1";
            flake = false;
        };

        declarative-cachix.url = "github:jonascarpay/declarative-cachix/master";
    };

    outputs = { self, nixpkgs, ... }@inputs:
        let
            linuxSystems = [ "x86_64-linux" "aarch64-linux" ];
            darwinSystems = [ "aarch64-darwin" "x86_64-darwin" ];
            forAllSystems = f: nixpkgs.lib.genAttrs (linuxSystems ++ darwinSystems) f;
            devShell = system: let pkgs = nixpkgs.legacyPackages.${system}; in {
            default = with pkgs; mkShell {
                nativeBuildInputs = with pkgs; [ fish git vim ];
                shellHook = "";
            };
        };
            mkMachines = { }: {
                rohan = inputs.home-manager.lib.homeManagerConfiguration {
                    configuration = { ... }: {
                        nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
                        imports = [ ./machines/rohan/home.nix ];
                    };
                    system = "x86_64-linux";
                    homeDirectory = "/home/rameezk";
                    username = "rameezk";
                };
                gondor = inputs.home-manager.lib.homeManagerConfiguration {
                    pkgs = import nixpkgs {
                        system = "aarch64-darwin";
                        overlays = [ inputs.emacs-overlay.overlay ];
                    };
                    modules = [
                        inputs.declarative-cachix.homeManagerModules.declarative-cachix
                        inputs.nixvim.homeManagerModules.nixvim
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

            darwinConfigurations."rivendell" = inputs.nix-darwin.lib.darwinSystem {
                system = "aarch64-darwin";
                specialArgs = inputs;
                modules = [
                    inputs.home-manager.darwinModules.home-manager
                    inputs.nix-homebrew.darwinModules.nix-homebrew
                    {
                        nix-homebrew = {
                            user = "rameezk";
                            enable = true;
                            taps = {
                                "homebrew/homebrew-core" = inputs.homebrew-core;
                                "homebrew/homebrew-cask" = inputs.homebrew-cask;
                                "homebrew/homebrew-bundle" = inputs.homebrew-bundle;
                            };
                            mutableTaps = false;
                            autoMigrate = true;
                        };
                    }
                    {
                        home-manager = {
                            sharedModules = [ 
                                inputs.nixvim.homeManagerModules.nixvim 
                                inputs.sops-nix.homeManagerModules.sops 
                            ];
                            users.rameezk = import ./machines/rivendell/home.nix;
                            extraSpecialArgs = { inherit inputs; };
                        };
                    }
                    ./machines/rivendell
                ];
            };
            darwinPackages = self.darwinConfigurations."rivendell".pkgs;

            devShells = forAllSystems devShell;
        };
}
