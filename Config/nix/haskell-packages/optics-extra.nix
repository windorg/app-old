{ mkDerivation, array, base, bytestring, containers, hashable
, indexed-profunctors, indexed-traversable-instances, lib, mtl
, optics-core, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "optics-extra";
  version = "0.4";
  sha256 = "3a48c7d9f7f5ac8960235cf0041f99f85d38b5597579fa7c817bf32c04f7d0fa";
  libraryHaskellDepends = [
    array base bytestring containers hashable indexed-profunctors
    indexed-traversable-instances mtl optics-core text transformers
    unordered-containers vector
  ];
  description = "Extra utilities and instances for optics-core";
  license = lib.licenses.bsd3;
}
