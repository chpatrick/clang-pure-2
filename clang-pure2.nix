{ mkDerivation, base, bytestring, Cabal, containers
, contravariant, hashable, inline-c, inline-c-cpp, lens, microlens
, microlens-contra, process, singletons, stdenv, stm
, template-haskell, unordered-containers, vector

, llvmPackages
}:
mkDerivation {
  pname = "clang-pure-2";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal process ];
  libraryHaskellDepends = [
    base bytestring containers contravariant inline-c inline-c-cpp microlens
    microlens-contra singletons stm template-haskell vector
    llvmPackages.clang-unwrapped llvmPackages.clang-unwrapped.lib
  ];
  executableHaskellDepends = [
    base bytestring hashable lens unordered-containers
  ];
  testHaskellDepends = [ base bytestring lens ];
  description = "Pure C++ code analysis with libclang";
  license = stdenv.lib.licenses.asl20;
}
