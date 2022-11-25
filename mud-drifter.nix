{ mkDerivation, async, attoparsec, base, binary, bytestring
, containers, directory, fgl, hpack, hspec
, hspec-attoparsec, lens, MissingH, mtl, network, network-simple
, parsec, pipes, pipes-attoparsec, pipes-binary, pipes-bytestring
, pipes-concurrency, pipes-network, pipes-parse, protolude
, QuickCheck, quickcheck-instances, regex-tdfa, safe-exceptions
, stdenv, suspend, text, text-show, time, timers, word8, arrows, heap
}:
mkDerivation {
  pname = "mud-drifter";
  version = "0.1.0.0";
  src = /home/tort/mud-drifter;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary bytestring containers directory
    fgl lens MissingH mtl network network-simple parsec pipes
    pipes-attoparsec pipes-binary pipes-bytestring pipes-concurrency
    pipes-network pipes-parse protolude regex-tdfa safe-exceptions
    suspend text text-show time timers word8 heap
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring directory pipes pipes-concurrency pipes-network
    protolude suspend text timers arrows
  ];
  testHaskellDepends = [
    base bytestring containers fgl hspec hspec-attoparsec parsec pipes
    pipes-bytestring protolude QuickCheck quickcheck-instances text
    text-show lens
  ];
  prePatch = "hpack";
  homepage = "https://github.com/tort/mud-drifter#readme";
  license = stdenv.lib.licenses.bsd3;
}
