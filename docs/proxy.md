# Proxy Configuration

## MacOS
On MacOS I found it easier to install `cntlm` via Homebrew as a Homebrew service. 
Once this is configured properly and running you need to provide the following configurations.

### Nix Daemon
For the Nix daemon on MacOS you need to configure the LaunchDaemon.

Copy the following LaunchDaemon [file](./org.nixos.nix-daemon.plist) to `/Library/Launchdaemons`.

Change the permissions of the file by:
```shell
chmod 644 /Library/LaunchDaemons/org.nixos.nix-daemon.plist
```
