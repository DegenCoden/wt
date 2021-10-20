{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  inputsFrom = [(import ./default.nix {})];
  buildInputs = with pkgs; [zlib] ++ (with haskellPackages; [
    cabal-install
    cabal2nix
    ghc
    haskell-language-server
    hlint
  ]);
}
