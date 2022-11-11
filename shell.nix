let
  pkgs = import <nixpkgs> { };
  typer_patched = pkgs.python311Packages.buildPythonPackage rec {
    pname = "typer";
    version = "0.7.0";
    format = "pyproject";
    nativeBuildInputs = [ pkgs.python311Packages.flit-core ];
    propagatedBuildInputs = [ pkgs.python311Packages.click ];
    src = pkgs.python311Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-/3l4RleKnyogG1NEKu3rVDMZRmhw++HHAeq2bddoEWU=";
    };
  };
in pkgs.mkShell rec {
  name = "dotfiles";
  buildInputs = with pkgs; [
    (python311.withPackages (ps: with ps; [ typer_patched ]))
    git-crypt
  ];
}
