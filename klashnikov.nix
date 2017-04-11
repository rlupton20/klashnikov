{ mkDerivation, base, concurrent-stack, haskell-etcd, http-client
, http-reverse-proxy, http-types, HUnit, protolude, stdenv
, test-framework, test-framework-hunit, text, tree-threads, wai
, warp, yaml, tyro
}:
mkDerivation {
  pname = "klashnikov";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base protolude text yaml ];
  executableHaskellDepends = [
    base concurrent-stack haskell-etcd http-client http-reverse-proxy
    http-types protolude tree-threads wai warp tyro
  ];
  testHaskellDepends = [
    base HUnit protolude test-framework test-framework-hunit text yaml
  ];
  homepage = "https://github.com/rlupton20/klashnikov#readme";
  license = stdenv.lib.licenses.gpl3;
}
