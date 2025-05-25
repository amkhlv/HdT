{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "hdt" ./. { }) (old: {
  postInstall =
    (old.postInstall or "")
    + "\n"
    + "mkdir -p $out/share/applications/; cp resources/hdt.desktop $out/share/applications/ ; \n";
})
