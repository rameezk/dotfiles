{ config, lib, pkgs, ... }:

with lib;

let cfg = config.java;
in {
  options.java = {
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
  };

  config = mkIf cfg.enable {
    home.packages = mkIf cfg.manageJDK [ pkgs.openjdk ];

    home.file.".gradle/gradle.properties".text = ''
      org.gradle.daemon=false
      systemProp.http.proxyHost=127.0.0.1
      systemProp.http.proxyPort=3128
      systemProp.https.proxyHost=127.0.0.1
      systemProp.https.proxyPort=3128
      org.gradle.jvmargs=-Xmx4096M
    '';
  };
}

