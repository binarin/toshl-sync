# Generated using stack2nix 0.1.3.0.
#
# Only works with sufficiently recent nixpkgs, e.g. "NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz".

{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, ghc ? pkgs.haskell.compiler.ghc802
}:

with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  stackPackages = { callPackage, pkgs, stdenv }:
self: {
      toshl-diff = callPackage ({ base-noprelude, classy-prelude, lens, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "toshl-diff";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            base-noprelude
            classy-prelude
            lens
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/githubuser/toshl-diff#readme";
          license = stdenv.lib.licenses.asl20;
        }) {};
    };
in
compiler.override {
  initialPackages = stackPackages;
  configurationCommon = { ... }: self: super: {};
}

