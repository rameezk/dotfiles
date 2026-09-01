{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/DeterminateSystems/nixpkgs-weekly/0.1";

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew = {
      url = "github:zhaofengli-wip/nix-homebrew";
      inputs.brew-src.follows = "brew-src";
    };
    brew-src = {
      url = "github:Homebrew/brew";
      flake = false;
    };
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
    homebrew-nikitabobko-tap = {
      url = "github:nikitabobko/homebrew-tap";
      flake = false;
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mysecrets = {
      url = "git+ssh://git@github.com/rameezk/nix-secrets.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rose-pine-starship = {
      url = "github:rose-pine/starship";
      flake = false;
    };
    rose-pine-fzf = {
      url = "github:rose-pine/fzf";
      flake = false;
    };
    rose-pine-tm-theme = {
      url = "github:rose-pine/tm-theme";
      flake = false;
    };
    rose-pine-fish = {
      url = "github:rose-pine/fish";
      flake = false;
    };

    auto-volume-toggler = {
      url = "github:rameezk/auto-volume-toggler";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mdmarks = {
      url = "github:rameezk/mdmarks";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    claude-skills = {
      url = "github:anthropics/skills";
      flake = false;
    };

    drawio-skill = {
      url = "github:jgraph/drawio-mcp";
      flake = false;
    };

    herdr-skill = {
      url = "github:herdrdev/herdr";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }@inputs:
    let
      linuxSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      darwinSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs (linuxSystems ++ darwinSystems) f;

      devShell =
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default =
            with pkgs;
            mkShell {
              nativeBuildInputs = with pkgs; [
                fish
                git
                vim
              ];
              shellHook = "";
            };
        };

      mkFormatter = system: nixpkgs.legacyPackages.${system}.nixfmt-tree;
    in
    {
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
                "nikitabobko/homebrew-tap" = inputs.homebrew-nikitabobko-tap;
              };
              mutableTaps = false;
              autoMigrate = true;
            };
          }
          {
            home-manager = {
              sharedModules = [
                inputs.nixvim.homeModules.nixvim
                inputs.sops-nix.homeManagerModules.sops
                inputs.catppuccin.homeManagerModules.catppuccin
              ];
              users.rameezk = import ./machines/rivendell/home.nix;
              extraSpecialArgs = {
                inherit inputs;
              };
            };
          }
          ./machines/rivendell
        ];
      };

      darwinConfigurations."fangorn" =
        let
          system = "aarch64-darwin";
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
        in
        inputs.nix-darwin.lib.darwinSystem {
          inherit system;
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
                  "nikitabobko/homebrew-tap" = inputs.homebrew-nikitabobko-tap;
                };
                mutableTaps = false;
                autoMigrate = true;
              };
            }
            {
              home-manager = {
                useGlobalPkgs = true;
                sharedModules = [
                  inputs.nixvim.homeModules.nixvim
                  inputs.sops-nix.homeManagerModules.sops
                  inputs.catppuccin.homeModules.catppuccin
                ];
                users.rameezk = import ./machines/fangorn/home.nix;
                extraSpecialArgs = {
                  inherit inputs pkgs;
                };
              };
            }
            ./machines/fangorn
          ];
        };

      devShells = forAllSystems devShell;

      formatter = forAllSystems mkFormatter;
    };
}
