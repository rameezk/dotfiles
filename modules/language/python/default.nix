{ pkgs, ... }: {

  home.packages = with pkgs; [
    (python312.withPackages (ps:
      with ps;
      [
        # pip # seems to be broken
        pipx # Yes, yes I know it's bad to install things globally.
      ]))
    stdenv.cc.cc.lib
  ];

}
