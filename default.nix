{ mkDerivation, base, brick, bytestring, http-client, lens
, lens-aeson, mtl, optparse-applicative, reflex, reflex-brick
, stdenv, text, wreq
}:
mkDerivation {
  pname = "reflex-brick-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brick bytestring http-client lens lens-aeson mtl
    optparse-applicative reflex reflex-brick text wreq
  ];
  homepage = "https://github.com/mmai/reflex-brick-example";
  license = stdenv.lib.licenses.bsd3;
}
