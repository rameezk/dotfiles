{ pkgs, ... }: { home.packages = with pkgs; [ kubectl kubectx stern k9s ]; }
