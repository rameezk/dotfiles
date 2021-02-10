# dotfiles

> A saner, more predictable way of managing my machines.

## Quickstart
At the moment, this is targetting only MacOS. 
In future, I hope to port this over to my other Linux machines as well.

1. Clone repo and cd into it
```bash
git clone git@github.com:rameezk/dotfiles.git ~/.dotfiles && cd ~/.dotfiles
```

2. Setup new machine build system
```bash
export PATH=$PATH:/usr/sbin
./bin/dot new-machine
```
When asked if you'd like to configure nix-darwin's configuration.nix, set the following:
```nix
programs.zsh.enable = true;
programs.fish.enable = true;
```

3. Build system and activate configuration
```bash
./bin/dot rebuild
```

## Maintenance

### Nuking environment
```bash
./maintenance/darwin-nuke.sh
```