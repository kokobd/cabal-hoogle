{pkgs, ...}: {
  name = "cabal-hoogle";
  compiler-nix-name = "ghc924"; # Version of GHC to use

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    p.musl64
  ];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  # shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
  # Non-Haskell shell tools go here
  shell.buildInputs = with pkgs; [
    nixpkgs-fmt
  ];
}
