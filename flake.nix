{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, ... }@inputs: {
    homeManagerConfigurations = {
      darwin = inputs.home-manager.lib.homeManagerConfiguration {
        configuration = ./home.nix;
        system = "x86_64-linux";
      };
    };

  };

}
