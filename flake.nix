{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        haskellTools = with pkgs; [
          cabal-install
          haskell-language-server
        ];
        backendShell =
          (pkgs.haskell.lib.addBuildTools pkgs.haskellPackages.jizhang haskellTools).envFunc
            { };
        flutterWeb = pkgs.flutter.override {
          supportedTargetFlutterPlatforms = [
            "universal"
            "web"
          ];
        };
        linuxPackages = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux (
          let
            backendPackage = pkgs.haskell.lib.justStaticExecutables (
              pkgs.haskell.lib.dontCheck pkgs.haskellPackages.jizhang
            );
            webAssets = import ./nix/web-assets.nix {
              inherit pkgs;
              src = pkgs.lib.fileset.toSource {
                root = ./client;
                fileset = pkgs.lib.fileset.unions [
                  ./client/lib
                  ./client/web
                  ./client/assets
                  ./client/pubspec.yaml
                  ./client/pubspec.lock
                ];
              };
              lockFile = ./client/pubspec.lock;
            };
          in
          {
            inherit webAssets;
          }
          // import ./nix/images.nix {
            inherit pkgs backendPackage webAssets;
          }
        );

      in
      with pkgs;
      {
        packages = {
          default = haskellPackages.jizhang;
          jizhang = haskellPackages.jizhang;
        }
        // linuxPackages;
        devShells = {
          backend = backendShell;
        }
        // {
          default =
            (pkgs.haskell.lib.addBuildTools pkgs.haskellPackages.jizhang (
              haskellTools
              ++ [
                pkgs.postgresql_17
                flutterWeb
                pkgs.gitMinimal
                pkgs.curl
                pkgs.python3
              ]
            )).envFunc
              { };
        };
      }
    )
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
            hself: hsuper: {
              jizhang = hself.callCabal2nix "jizhang" (final.lib.fileset.toSource {
                root = ./.;
                fileset = final.lib.fileset.unions [
                  ./jizhang.cabal
                  ./LICENSE
                  ./CHANGELOG.md
                  (final.lib.fileset.fileFilter (file: file.hasExt "hs") ./app)
                  (final.lib.fileset.fileFilter (file: file.hasExt "hs") ./src)
                  (final.lib.fileset.fileFilter (file: file.hasExt "hs") ./test)
                  (final.lib.fileset.fileFilter (file: file.hasExt "sql") ./sql)
                  (final.lib.fileset.fileFilter (
                    file: file.hasExt "js" || file.hasExt "css" || file.hasExt "html"
                  ) ./ui)
                  (final.lib.fileset.fileFilter (file: file.hasExt "json") ./data)
                  ./client/web/jizhang-mark.svg
                ];
              }) { };
            }
          );
        });
      };
    };
}
