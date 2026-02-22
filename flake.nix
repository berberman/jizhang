{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
        config = { allowBroken = true; };
      };
    in with pkgs; {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages.override {
            overrides = hself: hsuper: {
            };
          };
          jizhang = hpkgs.callCabal2nix "jizhang" ./. { };
        in with super;
        with haskell.lib; {
          inherit jizhang;
          jizhang-dev =
            addBuildTools jizhang [ cabal-install haskell-language-server postgresql ];
        };
      defaultPackage.x86_64-linux = jizhang;
      devShell.x86_64-linux = jizhang-dev.envFunc { };
    };
}
