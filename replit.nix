{pkgs}: {
  deps = [
    pkgs.vim
    pkgs.cabal-install
    pkgs.stack
    pkgs.zlib.dev
  ];
}
