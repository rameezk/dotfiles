{ pkgs, ... }: { home.packages = with pkgs; [ kubectl kubectx k9s ]; }
