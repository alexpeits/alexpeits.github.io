# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? null, compiler ? null }:

let

  nixpkgs = if isNull pkgs then
    import (import ./nix/sources.nix).nixpkgs-unstable {}
  else if builtins.typeOf pkgs == "set" then
    pkgs
  else
    import (builtins.getAttr pkgs (import ./nix/sources.nix)) {};

  haskellPackagesBase = if isNull compiler then
    nixpkgs.haskellPackages
  else
    nixpkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackagesBase.override {
    overrides = self: super:
      let
        hsPkgs = import ./nix/overrides.nix {
          pkgs = nixpkgs;
          self = self;
          super = super;
        };
        src = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
        drv = self.callCabal2nix "alexpeits" src {};
      in
        hsPkgs // { alexpeits = drv; };
  };

  site = nixpkgs.stdenv.mkDerivation {
    name = "alexpeits-github-io";
    buildInputs = [
      nixpkgs.python37Packages.pygments
      nixpkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    src = ./.;
    buildPhase = ''
      ${haskellPackages.alexpeits}/bin/alexpeits-exe
    '';
    installPhase = ''
      mkdir "$out"
      cp -R _build "$out/_build"
    '';
  };

  shell = haskellPackages.shellFor {
    packages = ps: [ ps.alexpeits ];
    buildInputs =
      [
        haskellPackages.ghcid
        haskellPackages.cabal-install
        nixpkgs.python37Packages.pygments
        nixpkgs.inotify-tools
      ];
    withHoogle = true;
  };

in
if nixpkgs.lib.inNixShell
then shell
else { site = site; alexpeits = haskellPackages.alexpeits; }
