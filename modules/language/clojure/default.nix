{ pkgs, lib, config, ... }: {
    options = {
        language.clojure.enable = lib.mkEnableOption "enable clojure";
    };

    config = lib.mkIf config.language.clojure.enable {
        home.packages = with pkgs; [ clojure clj-kondo leiningen clojure-lsp ];

        home.file.".lsp/config.edn".text = ''
            {:cljfmt {:remove-surrounding-whitespace? false}}
        '';
    };
}
