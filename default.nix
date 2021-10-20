{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./release.nix {}
