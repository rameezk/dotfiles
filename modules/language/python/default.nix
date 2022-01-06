{ pkgs, ... }: {

  home.packages = with pkgs; [
    # python 3.9 is specified in core module since it is needed to run the dot binary
    # python39
    stdenv.cc.cc.lib
    python39Packages.pip
    python39Packages.pipx
  ];

}
