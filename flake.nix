{

  description = "A saner, more predictable way of managing my machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # home-manager = {
    #   url = "github:rycee/home-manager/master";
    #   inputs.nixpkgs.follows = "/nixpkgs";
    # };

  };

  outputs = { self, darwin, nixpkgs }: {
    darwinConfigurations."rameezk-macbook" = darwin.lib.darwinSystem {
      modules = [ ./configuration.nix ];
    };
  };

  # outputs = { self, ... }@inputs: {

  #   homeManagerConfigurations = {
  #     rivendell = inputs.home-manager.lib.homeManagerConfiguration {
  #       configuration = ./hosts/rivendell/home.nix;
  #       system = "x86_64-darwin";
  #       homeDirectory = "/Users/rameezk";
  #       username = "rameezk";
  #     };
  #   };

  # };
}
