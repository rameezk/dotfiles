{ pkgs, ... }: {

  home.packages = with pkgs; [
    # python 3.10 is specified in core module since it is needed to run the dot binary
    # python310
    stdenv.cc.cc.lib
    (python310Packages.pip.overrideAttrs (_: {
      src = pkgs.python310Packages.fetchPypi {
        version = "22.0.2";
        pname = "pip";
        sha256 = "sha256-J7S3DDTsNfd5R/d3Bw2DMa27HkRIQumOcVDCiNwMrqQ=";
      };
    }))
    python310Packages.pipx
    pipenv
  ];

}
