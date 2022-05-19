let pkgs = import <nixpkgs> { };

in pkgs.mkShell rec {
  name = "dotfiles";
  buildInputs = with pkgs; [
    (python310.withPackages (ps: with ps; [ typer ]))
    git-crypt
  ];
}
