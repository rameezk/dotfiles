{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # outputs = { self, ... }@inputs: {
  #   homeManagerConfigurations = {
  #     machine = inputs.home-manager.lib.homeManagerConfiguration {
  #       configuration = ./home.nix;
  #       system = "x86_64-linux";
  #       homeDirectory = "/home/rameezk";
  #       username = "rameezk";
  #     };
  #   };

  # };

  outputs = { self, nixpkgs, home-manager, agenix, ... }@inputs: {
    workbook = home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        imports =
          [
            ./home.nix
            ./age.nix
          ];
        };

        system = "x86_64-linux";
        homeDirectory = "/home/rameezk";
        username = "rameezk";
      };

      rohan = self.workbook.activationPackage;
    };

  }
