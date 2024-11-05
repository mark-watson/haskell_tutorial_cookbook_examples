{pkgs}: {
  deps = [
    pkgs.haskellPackages.cpphs
    pkgs.sqlite-interactive
    pkgs.vim
    pkgs.cabal-install
    pkgs.stack
    pkgs.zlib.dev
  ];
}
