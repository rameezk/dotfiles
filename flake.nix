{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.home-manager = {
    url = "github:rycee/home-manager/master";
    inputs.nixpkgs.follows = "/nixpkgs";
  };

  outputs = { self, ... }@inputs: {

    homeManagerConfigurations = {
      rivendell = inputs.home-manager.lib.homeManagerConfiguration {
        configuration = ./hosts/rivendell/home.nix;
        system = "x86_64-darwin";
        homeDirectory = "/Users/rameezk";
        username = "rameezk";
      };
    };

  };
}