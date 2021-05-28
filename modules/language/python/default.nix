{ pkgs, ... }: {

  home.packages = with pkgs; [
    python39
    stdenv.cc.cc.lib
    python39Packages.pip
    python39Packages.pipx
  ];

}
