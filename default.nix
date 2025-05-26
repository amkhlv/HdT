{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "hdt" ./. { }) (old: {
  postInstall =
    (old.postInstall or "")
    + "\n"
    + "mkdir -p $out/share/applications/; mkdir $out/share/icons/ ;"
    + "cp resources/hdt.desktop $out/share/applications/ ;"
    + "cp resources/hdt.svg $out/share/icons/ ;"
    + "echo Icon=$out/share/icons/hdt.svg >> $out/share/applications/hdt.desktop ; \n";
})
