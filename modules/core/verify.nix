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
          "ssh_key"
          "gpg_key"
          "file_permissions"
        ];
        description = ''
          Type of check:
          - 'command': checks if a command exists
          - 'file': checks if a file exists
          - 'ssh_key': validates SSH key and shows fingerprint (safe)
          - 'gpg_key': validates GPG key is importable
          - 'file_permissions': checks file exists with specific permissions
        '';
      };

      name = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Name of the command to check (for type='command')";
      };

      path = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to the file to check (for type='file', 'ssh_key', 'gpg_key', 'file_permissions')";
      };

      permissions = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Expected octal permissions (for type='file_permissions'), e.g. '600'";
      };

      desc = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable description of what is being checked";
      };
    };
  };

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
    else if check.type == "ssh_key" then
      ''
        set expanded_path (eval echo ${check.path})
        if test -e "$expanded_path"
          set fingerprint (${pkgs.openssh}/bin/ssh-keygen -lf "$expanded_path" 2>/dev/null)
          if test $status -eq 0
            set passed (math $passed + 1)
            echo -e "$green✓$reset ${check.desc}"
            echo -e "    Fingerprint: $fingerprint"
          else
            set failed (math $failed + 1)
            echo -e "$red✗$reset ${check.desc} (invalid key format)"
          end
        else
          set failed (math $failed + 1)
          echo -e "$red✗$reset ${check.desc} (file not found: ${check.path})"
        end
      ''
    else if check.type == "gpg_key" then
      ''
        set expanded_path (eval echo ${check.path})
        if test -e "$expanded_path"
          set gpg_output (${pkgs.gnupg}/bin/gpg --dry-run --import "$expanded_path" 2>&1)
          if test $status -eq 0
            set passed (math $passed + 1)
            echo -e "$green✓$reset ${check.desc}"
            echo "$gpg_output" | ${pkgs.gnugrep}/bin/grep -E "^gpg:" | head -1 | while read line
              echo -e "    $line"
            end
          else
            set failed (math $failed + 1)
            echo -e "$red✗$reset ${check.desc} (invalid GPG key)"
          end
        else
          set failed (math $failed + 1)
          echo -e "$red✗$reset ${check.desc} (file not found: ${check.path})"
        end
      ''
    else if check.type == "file_permissions" then
      ''
        set expanded_path (eval echo ${check.path})
        if test -e "$expanded_path"
          set resolved_path (realpath "$expanded_path" 2>/dev/null; or echo "$expanded_path")
          set actual_perms (stat -Lf %Lp "$resolved_path" 2>/dev/null; or stat -Lc %a "$resolved_path" 2>/dev/null)
          if test "$actual_perms" = "${check.permissions}"
            set passed (math $passed + 1)
            echo -e "$green✓$reset ${check.desc} (${check.permissions})"
          else
            set failed (math $failed + 1)
            echo -e "$red✗$reset ${check.desc} (expected ${check.permissions}, got $actual_perms)"
          end
        else
          set failed (math $failed + 1)
          echo -e "$red✗$reset ${check.desc} (file not found: ${check.path})"
        end
      ''
    else
      "";

  verifyScript = pkgs.writeScriptBin "verify" ''
    #!${pkgs.fish}/bin/fish

    set green (set_color green)
    set red (set_color red)
    set yellow (set_color yellow)
    set reset (set_color normal)
    set bold (set_color --bold)

    set passed 0
    set failed 0

    echo ""
    echo "$bold""Post-rebuild verification""$reset"
    echo "─────────────────────────────"
    echo ""

    ${lib.concatStringsSep "\n" (map generateCheck cfg.checks)}

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
