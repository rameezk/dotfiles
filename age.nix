{ config, lib, pkgs, inputs, ...}:

let inherit (inputs) agenix;
  secretsDir = "${toString ../hosts}/${config.networking.hostName}/secrets";
  secretsFile = "${secretsDir}/secrets.nix";
in
{
  imports = [ agenix.nixosModules.age ];
  age.secrets.secret1.file = ../secrets/secret1.age;
}
