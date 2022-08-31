{ pkgs, ... }:

{
  home.file.".proxyrc".text = ''
    export HTTP_PROXY=http://localhost:3128
    export https_proxy=http://localhost:3128
    export HTTPS_PROXY=http://localhost:3128
    export ftp_proxy=http://localhost:3128
    export FTP_PROXY=http://localhost:3128
    export all_proxy=http://localhost:3128
    export ALL_PROXY=http://localhost:3128
    export NIX_CURL_FLAGS="-x $proxy"
    export no_proxy=localhost,127.0.0.1
    export NO_PROXY=localhost,127.0.0.1
  '';
}
