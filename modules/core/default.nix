{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      # Needed to run dot binary
      (python311.withPackages (ps:
        with ps;
        [
          (buildPythonPackage rec {
            pname = "typer";
            version = "0.7.0";
            format = "pyproject";
            nativeBuildInputs = [ flit-core ];
            propagatedBuildInputs = [ click ];
            src = fetchPypi {
              inherit pname version;
              sha256 = "sha256-/3l4RleKnyogG1NEKu3rVDMZRmhw++HHAeq2bddoEWU=";
            };
          })
        ]))
    ];
}
