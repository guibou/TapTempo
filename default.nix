with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "taptempo";

  myghc = haskellPackages.ghcWithPackages(pkgs : with pkgs; [optparse-applicative refined process clock formatting shakespeare]);

  src = ./.;

  nativeBuildInputs = [ myghc git ];

  buildPhase = ''
    ghc -O2 -Wall Main.hs

    mkdir -p $out/bin
  '';

  installPhase = ''
    cp Main $out/bin/TapTempo
    '';
}
