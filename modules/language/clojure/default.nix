{ pkgs, ... }: {

  home.packages = with pkgs; [ clojure clj-kondo leiningen clojure-lsp ];

  home.file.".lsp/config.edn".text = ''
    {:cljfmt {:remove-surrounding-whitespace? false}}
  '';

}
