with (import <nixpkgs> { });
mkShell rec {
  buildInputs = [
    cabal-install
    ghc
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
    dav1d
    libxml2
  ];
  nativeBuildInputs = [
    wrapGAppsHook4
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
