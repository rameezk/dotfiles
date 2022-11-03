{ pkgs, ... }:

let
  proxy_protocol = "http";
  proxy_host = "localhost";
  proxy_port = "3128";
  proxy_full_addr = "${proxy_protocol}://${proxy_host}:${proxy_port}/";
in {
  home.file.".proxyrc".text = ''
    export HTTP_PROXY=${proxy_full_addr}
    export http_proxy=${proxy_full_addr}
    export https_proxy=${proxy_full_addr}
    export HTTPS_PROXY=${proxy_full_addr}
    export ftp_proxy=${proxy_full_addr}
    export FTP_PROXY=${proxy_full_addr}
    export all_proxy=${proxy_full_addr}
    export ALL_PROXY=${proxy_full_addr}
    export NIX_CURL_FLAGS="-x ${proxy_full_addr}"
    export no_proxy=localhost,127.0.0.1
    export NO_PROXY=localhost,127.0.0.1
  '';

  home.file.".m2/settings.xml".text = ''
    <settings>
        <proxies>
            <proxy>
                <id>proxy</id>
                <active>true</active>
                <protocol>${proxy_protocol}</protocol>
                <host>${proxy_host}</host>
                <port>${proxy_port}</port>
                <nonProxyHosts>localhost|127.0.0.1</nonProxyHosts>
            </proxy>
        </proxies>
    </settings>
  '';
}
