{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        compilers = [ "ghc8107" "ghc926" "ghc944" ];
        defaultCompiler = "ghc8107";

        mkShell = compiler: pkgs.mkShell {
          buildInputs = with pkgs; [
            zlib.dev
          ];
          packages = with pkgs; [
            cabal-install
            haskell.compiler.${compiler}
            ormolu
            miniserve
          ] ++ (with haskell.packages.${compiler}; [
            haskell-language-server
            cabal-fmt
          ]);
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
