let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackageOld: rec {

          collection-json =
            haskellPackagesNew.callPackage ./collection-json.nix { };

          collection-server =
            haskellPackagesNew.callPackage ./default.nix { };

        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { collection-server = pkgs.haskellPackages.collection-server;
  }
