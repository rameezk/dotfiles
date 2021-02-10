{
  description = "A saner, more predictable way of managing my machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = github:rycee/home-manager/master;
      inputs.nixpkgs.follows = "/nixpkgs";
    };
  };

  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs: {
    # workbook = home-manager.lib.homeManagerConfiguration {
    #   configuration = { ... }: {
    #     imports = [
    #       ./hosts/rivendell/home.nix
    #     ];
    #   };

    #   system = "x86_64-darwin";
    #   homeDirectory = "/Users/rameezk";
    #   username = "rameezk";
    # };

    # macbook = self.workbook.activationPackage;

    darwinConfigurations."rameezk-macbook" = darwin.lib.darwinSystem {
      modules = [ 
        ./configuration.nix
        home-manager.lib.homeManagerConfiguration
        {
          configuration = { ... }: {
            imports = [
              ./hosts/rivendell/home.nix2
            ];
          };

          system = "x86_64-darwin";
          homeDirectory = "/Users/rameezk";
          username = "rameezk";
        }
      ];
    };
  };
}