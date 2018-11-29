{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
} : 
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ./nix/reflex-platform.nix;
  reflex-brick-sources = import ./nix/reflex-brick.nix;

  modifiedHaskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: {
      reflex-brick = self.callPackage reflex-brick-sources {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./. {};

in
  if pkgs.lib.inNixShell then drv.env else drv
