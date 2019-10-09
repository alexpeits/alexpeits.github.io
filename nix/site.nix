# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs, lib, alexpeits-github-io }:

pkgs.stdenv.mkDerivation {
  name = "alexpeits-github-io-site";
  src = lib.sourceByRegex ../. [
    ".*.md"
    ".*.html"
    "posts"
    "templates"
    "css"
  ];
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  buildInputs = [ alexpeits-github-io ];
  preConfigure = ''
    export LANG="en_US.UTF-8";
  '';

  buildPhase = ''
    site build
  '';

  installPhase = ''
    cp -R _site $out
  '';
}
