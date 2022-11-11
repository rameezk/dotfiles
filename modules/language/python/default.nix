{ pkgs, ... }: {

  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        pip
        pipx # Yes, yes I know it's bad to install things globally.
      ]))
    pipenv
    stdenv.cc.cc.lib
  ];

}
