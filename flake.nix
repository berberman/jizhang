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
    eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in
      with pkgs;
      {
        packages.default = haskellPackages.jizhang;
        packages.jizhang = haskellPackages.jizhang;
        devShells.default =
          with haskell.lib;
          (addBuildTools (haskellPackages.jizhang) [
            cabal-install
            haskell-language-server
            postgresql
            typescript-language-server
          ]).envFunc
            { };
      }
    )
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
            hself: hsuper: {
              jizhang = hself.callCabal2nix "jizhang" ./. { };
            }
          );
        });
      };
    };
}
