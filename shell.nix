with (import <nixpkgs> {});
mkShell rec {
  buildInputs = [
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
              libselinux
              libsepol
              poppler
              poppler_gi
              libsysprof-capture
              lerc
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
