{ config, lib, pkgs, inputs, ...}:

{
  imports = [ agenix.nixosModules.age ];
  # age.secrets.secret1.file = ../secrets/secret1.age;
}
