{ options, config, inputs, lib, pkgs, ... }:

let inherit (inputs) agenix;
  secretsDir = "./secrets";
  secretsFile = "${secretsDir}/secrets.nix";
in
{
  imports = [ agenix.nixosModules.age ];
  environment.systemPackages = [ agenix.defaultPackage.x86_64-linux ];
}
