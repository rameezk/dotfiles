{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  firefox-addons-allowUnfree = pkgs.callPackage inputs.firefox-addons { };
in
{
  options = {
    browser.firefox.enable = lib.mkEnableOption "enable firefox";
  };

  config = lib.mkIf config.browser.firefox.enable {
    programs.firefox = {
      enable = true;

      languagePacks = [ "en-GB" ];

      profiles = {
        default = {
          name = "default";

          id = 0;
          isDefault = true;

          settings = {
            "browser.startup.homepage" = "https://start.duckduckgo.com";
            "extensions.autoDisableScopes" = 0;
          };

          search = {
            default = "ddg";
            force = true;
          };

          extensions = {
            packages = with firefox-addons-allowUnfree; [
              ublock-origin
              tridactyl
              onepassword-password-manager
            ];
          };
        };
      };
    };
  };
}
