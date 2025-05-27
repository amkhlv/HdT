{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "hdt" ./. { }) (old: {
  postInstall =
    (old.postInstall or "")
    + "\n"
    + "mkdir -p $out/share/applications/; mkdir -p $out/share/icons/hicolor/scalable/apps/ ;"
    + "cp resources/HdTPDFViewer.desktop $out/share/applications/ ;"
    + "cp resources/HdTPDFViewer.svg $out/share/icons/hicolor/scalable/apps/ ;"
    + "\n";
})
