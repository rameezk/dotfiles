{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.verify;

  checkType = lib.types.submodule {
    options = {
      type = lib.mkOption {
        type = lib.types.enum [
          "command"
          "file"
        ];
        description = "Type of check: 'command' checks if a command exists, 'file' checks if a file exists";
      };

      name = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Name of the command to check (for type='command')";
      };

      path = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to the file to check (for type='file')";
      };

      desc = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable description of what is being checked";
      };
    };
  };

  # Generate fish script for a single check
  generateCheck =
    check:
    if check.type == "command" then
      ''
        if type -q ${check.name}
          set passed (math $passed + 1)
          echo -e "$green✓$reset ${check.desc} (${check.name})"
        else
          set failed (math $failed + 1)
          echo -e "$red✗$reset ${check.desc} (${check.name})"
        end
      ''
    else if check.type == "file" then
      ''
        set expanded_path (eval echo ${check.path})
        if test -e "$expanded_path"
          set passed (math $passed + 1)
          echo -e "$green✓$reset ${check.desc} (${check.path})"
        else
          set failed (math $failed + 1)
          echo -e "$red✗$reset ${check.desc} (${check.path})"
        end
      ''
    else
      "";

  # Generate the complete verification script
  verifyScript = pkgs.writeScriptBin "verify" ''
    #!${pkgs.fish}/bin/fish

    # Colors
    set green (set_color green)
    set red (set_color red)
    set yellow (set_color yellow)
    set reset (set_color normal)
    set bold (set_color --bold)

    # Counters
    set passed 0
    set failed 0

    echo ""
    echo "$bold""Post-rebuild verification""$reset"
    echo "─────────────────────────────"
    echo ""

    # Run all checks
    ${lib.concatStringsSep "\n" (map generateCheck cfg.checks)}

    # Summary
    set total (math $passed + $failed)
    echo ""
    echo "─────────────────────────────"
    if test $failed -eq 0
      echo -e "$green✓ All $total checks passed$reset"
      exit 0
    else
      echo -e "$yellow$passed passed$reset, $red$failed failed$reset out of $total checks"
      exit 1
    end
  '';

in
{
  options.verify = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable post-rebuild verification";
    };

    checks = lib.mkOption {
      type = lib.types.listOf checkType;
      default = [ ];
      description = "List of verification checks to run";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ verifyScript ];
  };
}
