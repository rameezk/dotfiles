{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      # Needed to run dot binary
      (python39.withPackages (ps: with ps; [ typer ]))
    ];
}
