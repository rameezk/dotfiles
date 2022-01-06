{ pkgs, ... }: {
  home.packages = with pkgs; [ nixfmt python39 python39Packages.typer ];
}
