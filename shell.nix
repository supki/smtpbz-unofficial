{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc984
, stack ? pkgs.stack
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    zlib
  ];

  shellHook = ''
    chmod go-w smtpbz smtpbz-bin smtpbz/.ghci smtpbz-bin/.ghci

    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}"
  '';
}
