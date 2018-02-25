{ mkDerivation, aeson, base, collection-json, directory, envy
, exceptions, extra, filepath, hspec, http-api-data, http-media
, http-types, network-arbitrary, network-uri, QuickCheck
, quickcheck-instances, servant, servant-server, stdenv
, test-invariant, text, wai-cors, wai-extra, warp, yaml
}:
mkDerivation {
  pname = "collection-server";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base collection-json directory envy exceptions extra filepath
    http-api-data http-media http-types network-uri servant
    servant-server text wai-cors wai-extra warp yaml
  ];
  testHaskellDepends = [
    aeson base collection-json directory exceptions extra filepath
    hspec http-api-data http-media network-arbitrary network-uri
    QuickCheck quickcheck-instances servant servant-server
    test-invariant text yaml
  ];
  homepage = "https://github.com/alunduil/collection-server";
  description = "Static Resource Server for application/vnd.collection+json";
  license = stdenv.lib.licenses.mit;
}
