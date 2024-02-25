{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        compilers = [ "ghc92" "ghc94" "ghc96" ];
        defaultCompiler = "ghc94";

        mkShell = compiler: pkgs.mkShell {
          buildInputs = with pkgs; [
            zlib.dev
          ];
          packages = with pkgs; [
            cabal-install
            haskell.compiler.${compiler}
            ormolu
            miniserve
          ] ++ (with haskell.packages.${compiler};
            (lib.lists.optional (compiler != "ghc96") haskell-language-server) ++
              [ cabal-fmt ghcid ]
          );
          shellHook = ''
            export CABAL_DIR=$(pwd)/.cabal
            export CABAL_CONFIG=$(pwd)/.cabal/config
          '';
        };
      in
      {
        devShells = {
          default = mkShell defaultCompiler;
        } // builtins.listToAttrs (
          builtins.map
            (compiler: {
              name = compiler;
              value = mkShell compiler;
            })
            compilers
        );
      }
    );
}
