{ mkDerivation, array, base, bytestring, containers, criterion
, indexed-profunctors, inspection-testing, lens, lib, mtl
, optics-core, optics-extra, optics-th, QuickCheck, random, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "optics";
  version = "0.4";
  sha256 = "051abd36d2914c56b3ac6d15a3ff425dbd0cc774f9530ff0d258ece355750da2";
  libraryHaskellDepends = [
    array base containers mtl optics-core optics-extra optics-th
    transformers
  ];
  testHaskellDepends = [
    base containers indexed-profunctors inspection-testing mtl
    optics-core QuickCheck random tasty tasty-hunit tasty-quickcheck
    template-haskell
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion lens transformers
    unordered-containers vector
  ];
  description = "Optics as an abstract interface";
  license = lib.licenses.bsd3;
}
