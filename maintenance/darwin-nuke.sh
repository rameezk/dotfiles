#!/usr/bin/env zsh

echo "Nuking nix things in home directory"
rm -rf $HOME/{.nix-channels,.nix-defexpr,.nix-profile,.config/nixpkgs}

echo "Nuking /nix"
sudo rm -rf /nix

echo "Open diskutil and delete /nix volume. Close all terminals first."
echo "Done."