{ config, lib, pkgs, ... }:

with lib;

let cfg = config.language.java;
in {
  options.language.java = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable Java on this machine";
    };

    manageJDK = mkOption {
      type = types.bool;
      default = true;
      description = "Download and manage the Java JDK";
    };

    enableProxy = mkOption {
      type = types.bool;
      default = false;
      description = "Enable proxy";
    };

    proxyProtocol = mkOption {
      type = types.enum [ "http" "https" ];
      default = "http";
      description = "Proxy protocol to be used. Either http or https";
    };

    proxyHost = mkOption {
      type = types.str;
      default = "";
      description = "Proxy host to be used for downstream proxy";
    };

    proxyPort = mkOption {
      type = types.int;
      default = 8080;
      description = "Proxy port to be used for downstream proxy";
    };
  };

  config = mkIf cfg.enable (let

    gradleConfig = ''
      org.gradle.daemon=false
      org.gradle.jvmargs=-Xmx4096M

      ${optionalString cfg.enableProxy ''
        systemProp.http.proxyHost=${cfg.proxyHost}
        systemProp.http.proxyPort=${toString cfg.proxyPort}
        systemProp.https.proxyHost=${cfg.proxyHost}
        systemProp.https.proxyPort=${toString cfg.proxyPort}
      ''}
    '';

    mavenConfig = ''
      <settings>
      ${optionalString cfg.enableProxy ''
        <proxies>
            <proxy>
                <id>proxy</id>
                <active>true</active>
                <protocol>${cfg.proxyProtocol}</protocol>
                <host>${cfg.proxyHost}</host>
                <port>${toString cfg.proxyPort}</port>
                <nonProxyHosts>localhost|127.0.0.1</nonProxyHosts>
            </proxy>
        </proxies>
      ''}
      </settings>
    '';

  in {

    home.file.".gradle/gradle.properties".text = gradleConfig;

    home.file.".m2/settings.xml".text = mavenConfig;

    home.packages = mkIf cfg.manageJDK [ pkgs.openjdk ];

  });
}

