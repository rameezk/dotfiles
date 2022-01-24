{ pkgs, ... }: {

  home.packages = with pkgs; [
    # python 3.10 is specified in core module since it is needed to run the dot binary
    # python310
    stdenv.cc.cc.lib
    python310Packages.pip
    python310Packages.pipx
  ];

}
