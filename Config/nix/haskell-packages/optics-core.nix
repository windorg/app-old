{ mkDerivation, array, base, containers, indexed-profunctors
, indexed-traversable, lib, transformers
}:
mkDerivation {
  pname = "optics-core";
  version = "0.4";
  sha256 = "3ef75c4cb04c6a327ae854c426f4642ae3bf2ee4192a6f52f5b21305bf6bddcf";
  libraryHaskellDepends = [
    array base containers indexed-profunctors indexed-traversable
    transformers
  ];
  description = "Optics as an abstract interface: core definitions";
  license = lib.licenses.bsd3;
}
