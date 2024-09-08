{pkgs}: {
  deps = [
    pkgs.sqlite-interactive
    pkgs.vim
    pkgs.cabal-install
    pkgs.stack
    pkgs.zlib.dev
  ];
}
