{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      # Needed to run dot binary
      (python310.withPackages (ps: with ps; [ typer ]))
    ];
}
