with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "taptempo";

  myghc = haskellPackages.ghcWithPackages(pkgs : with pkgs; [optparse-applicative refined process here]);
  buildInputs = [ myghc ];
}
