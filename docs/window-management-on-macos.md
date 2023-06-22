# Window Management in MacOS

## skhd

> A keyboard daemon for MacOS

To setup a launchagent (background service) for skhd to the following:
* Find the local nix store path `nix eval --raw nixpkgs#skhd`
* Navigate there, and copy the plist file to your local launchagent directory `cp ./Library/LaunchDaemons/org.nixos.skhd.plist /Library/LaunchAgents`.
