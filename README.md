# reflex-brick-example

**Work in progress**

A small console application in Haskell made with [brick](https://github.com/jtdaugherty/brick) and [reflex](https://reflex-frp.org/) with the help of [reflex-brick](https://github.com/dalaing/reflex-brick).

You need _[nix](https://nixos.org/nix/)_ installed on your system :

`curl https://nixos.org/nix/install | sh`

then compile and run with :

```
cd reflex-brick-example
nix-shell
nix-env -i cabal-install # installs cabal if not already installed
cabal build
cabal run
```
