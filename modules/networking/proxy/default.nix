{ config, lib, pkgs, ... }:

with lib;

let cfg = config.proxy;
in {
  options = {
    proxy = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the proxy module";
      };

      protocol = mkOption {
        type = types.enum [ "http" "https" ];
        description = "Protocol to be used. Either http or https";
      };

      host = mkOption {
        type = types.str;
        description = "Host to be used for downstream proxy";
      };

      port = mkOption {
        type = types.int;
        description = "Port to be used for downstream proxy";
      };
    };
  };

  config = mkIf cfg.enable (let
    proxy_full_addr = "${cfg.protocol}://${cfg.host}:${toString cfg.port}/";
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
              <protocol>${cfg.protocol}</protocol>
              <host>${cfg.host}</host>
              <port>${toString cfg.port}</port>
              <nonProxyHosts>localhost|127.0.0.1</nonProxyHosts>
          </proxy>
      </proxies>
      </settings>
    '';

  });
}
