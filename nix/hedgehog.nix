{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, fail
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stm, template-haskell, text, time
, transformers, transformers-base, wl-pprint-annotated
, stdenv
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.0.1";
  sha256 = "1qc7hkqbnsk3f5r26wc35r3qiy941nmcxhfqgcq9027kw4gs0bi0";
  revision = "1";
  editedCabalFile = "0dq3ry7py2wsiwxar11zbvm3xmifm92nx4bh61lqxzmpwyyiwnxn";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions fail lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show semigroups text transformers
  ];
  description = "Release with confidence";
  license = stdenv.lib.licenses.bsd3;
}
