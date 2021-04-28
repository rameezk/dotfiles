let 
  pkgs = import <nixpkgs> {};

in pkgs.mkShell rec {
  name = "dotfiles";
  buildInputs = with pkgs; [
    nixfmt
    python
  ];
}
