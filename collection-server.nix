let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackageOld:
          let
            toPackage = file: _: {
              name = builtins.replaceStrings [ ".nix" ] [ "" ] file;

              value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
            };

            packages = pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          in
            packages // {
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
