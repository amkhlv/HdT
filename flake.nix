{ 
  description = "HdT pdf viewer";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
      flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              cabal-install
              haskell-language-server
              cabal2nix
              pkg-config
              zlib
              zlib.dev
              bintools
              glib
              cairo
              expat
              pcre2
              pcre.dev
              hyperscan
              xorg.libXdmcp
              xorg.libXtst
              pango
              util-linux.dev
              fribidi
              libthai
              libdatrie
              gtk4
              libxkbcommon
              libepoxy
              icu
              gmp
              zlib
              haskellPackages.zlib
              haskellPackages.gi-gtk_4
              librsvg
              poppler
              poppler_gi
              libsysprof-capture
              libselinux
              libsepol
              lerc
              wrapGAppsHook4
            ];
          };
        }
      );
}


