{
  description = "Rameez's configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    nur.url = "github:nix-community/NUR";
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, nur, ... }@inputs: {
    workbook = home-manager.lib.homeManagerConfiguration {
      configuration = { ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay nur.overlay ];

        imports = let
          nur-no-pkgs = import nur {
            nurpkgs = import nixpkgs { system = "x86_64-linux"; };
          };
        in [ nur-no-pkgs.repos.rycee.hmModules.emacs-init ./home.nix ];
      };

      system = "x86_64-linux";
      homeDirectory = "/home/rameezk";
      username = "rameezk";
    };

    rohan = self.workbook.activationPackage;
  };
}
