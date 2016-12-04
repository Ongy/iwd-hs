# iwd-hs
This is a library/tool to interact with [iwd](https://git.kernel.org/cgit/network/wireless/iwd.git/).


## What works in TUI (and therefore library)
 * Query objects from Dbus
 * React to connection/status change
 * Trigger connect
 * Trigger disconnect
 * Enter passphrase
 
### Using the tui
The tui builds one list of networks for each interface managed by your iwd. 
 * To switch interface press **Tab**. 
 * To connect to a network press **Enter**.
 * To disconnect a device, select it and press **d**.

## Building
If you are new to haskell, you need *ghc* and *cabal-install*.
Then to build this, clone this repository then
> cd iwd-hs

> cabal sandbox init

> cabal install --dependencies-only

> cabal build

To run use: 
> cabal run iwd-tui

# Currently missing in tui
 * React to interface hotplug (I don't have the kernel, so can't test)
 * React to scan results/Network (dis)appearing
