with import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/1bc5bf4beb759e563ffc7a8a3067f10a00b45a7d.tar.gz") {};
let 
  drv = (haskellPackages.callCabal2nix "TapTempo" ./. {}).overrideAttrs(oldAttrs :
  {
    buildInputs = oldAttrs.buildInputs ++ [ git ];
  });
in
if lib.inNixShell then drv.env else drv

