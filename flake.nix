{
  description = "Check big coercions";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    let utils = flake-utils.lib;
    in
    utils.eachDefaultSystem (system:
    let
      supportedGHCVersion = "884";
      compilerVersion = "ghc${supportedGHCVersion}";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          coercion-check = hfinal.callCabal2nix "coercion-check" ./. { };
        };
      };
    in
    rec {
      packages = utils.flattenTree
        { coercion-check = hsPkgs.coercion-check; };

      # nix develop
      devShell = hsPkgs.shellFor {
        withHoogle = true;
        packages = p: [
          p.coercion-check
        ];
        buildInputs = with pkgs; [
          hsPkgs.haskell-language-server
          haskellPackages.cabal-install
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
        ] ++ (builtins.attrValues (import ./scripts.nix { s = pkgs.writeShellScriptBin; }));
      };

      # nix build
      defaultPackage = packages.coercion-check;
    });
}
