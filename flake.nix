{
  description = "Provide warnings for unexpected Core generation";

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
          core-warn = hfinal.callCabal2nix "core-warn" ./. { };
        };
      };
    in
    rec {
      packages = utils.flattenTree
        { core-warn = hsPkgs.core-warn; };

      # nix develop
      devShell = hsPkgs.shellFor {
        packages = p: [
          p.core-warn
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
      defaultPackage = packages.core-warn;
    });
}
