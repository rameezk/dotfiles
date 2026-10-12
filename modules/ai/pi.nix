{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
let
  cfg = config.ai.pi;
  piPkg = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
  piPackages = [ "npm:@gotgenes/pi-anthropic-auth" ];
in
{
  options.ai.pi = {
    enable = lib.mkEnableOption "pi coding agent (pi.dev)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ piPkg ];

    # Install pi extensions (idempotent; keeps ~/.pi/agent/settings.json mutable)
    home.activation.piPackages = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      export PATH="${pkgs.nodejs}/bin:${pkgs.git}/bin:$PATH"
      ${lib.concatMapStringsSep "\n" (p: ''
        run ${piPkg}/bin/pi install ${p} || echo "pi install ${p} failed"
      '') piPackages}
    '';
  };
}
