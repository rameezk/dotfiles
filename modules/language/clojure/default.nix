{ pkgs, ... }: { home.packages = with pkgs; [ clojure clj-kondo ]; }
