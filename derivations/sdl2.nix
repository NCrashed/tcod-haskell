{ mkDerivation, base, bytestring, deepseq, exceptions, linear, SDL2
, StateVar, stdenv, text, transformers, vector, weigh
}:
mkDerivation {
  pname = "sdl2";
  version = "2.5.0.0";
  sha256 = "883bcc6967194ca2a5d69067b85efccf7f6ba40e7237d4371bab94d6a04766f4";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring exceptions linear StateVar text transformers vector
  ];
  librarySystemDepends = [ SDL2 ];
  libraryPkgconfigDepends = [ SDL2 ];
  testHaskellDepends = [ base deepseq linear vector weigh ];
  description = "Both high- and low-level bindings to the SDL library (version 2.0.6+).";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
