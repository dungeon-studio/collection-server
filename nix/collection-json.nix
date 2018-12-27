{ mkDerivation, aeson, base, bytestring, fetchgit, hspec
, network-uri, QuickCheck, quickcheck-instances, stdenv
, test-invariant, text
}:
mkDerivation {
  pname = "collection-json";
  version = "1.1.0.1";
  src = fetchgit {
    url = "https://github.com/alunduil/collection-json.hs";
    sha256 = "00jfc1mrglsk7ngjd3h13kkwa1acch0rq9q9z1z1m67nq5q6aykw";
    rev = "e611e5cb6fe471412412be502a252504fbd3ce25";
  };
  libraryHaskellDepends = [ aeson base network-uri text ];
  testHaskellDepends = [
    aeson base bytestring hspec network-uri QuickCheck
    quickcheck-instances test-invariant text
  ];
  homepage = "https://github.com/alunduil/collection-json.hs";
  description = "Collection+JSON—Hypermedia Type Tools";
  license = stdenv.lib.licenses.mit;
}
