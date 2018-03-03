with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "taptempobf";

  myghc = haskellPackages.ghcWithPackages(pkgs : with pkgs; [megaparsec clock]);

  src = ./.;

  nativeBuildInputs = [ myghc ];

  buildPhase = ''
    ghc -O2 -Wall BfMain.hs
    ghc -O2 -Wall TapTempoBF.hs

    mkdir -p $out/bin
  '';

  installPhase = ''
    cp BfMain $out/bin/Bf
    cp TapTempoBF $out/bin/TapTempoBF
    '';
}
