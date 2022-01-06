{ pkgs, ... }: {
  home.packages = with pkgs; [
    nixfmt
    (python39.withPackages (ps: with ps; [ typer ]))
  ];
}
