{ mkDerivation, aeson, base, collection-json, directory, envy
, extra, filepath, http-api-data, http-media, http-types, MissingH
, network-uri, servant, servant-server, stdenv, text, wai-logger
, warp, yaml
}:
mkDerivation {
  pname = "collection-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base collection-json directory envy extra filepath
    http-api-data http-media http-types MissingH network-uri servant
    servant-server text wai-logger warp yaml
  ];
  homepage = "https://github.com/alunduil/collection-server";
  description = "Static Resource Server for application/vnd.collection+json";
  license = stdenv.lib.licenses.mit;
}
