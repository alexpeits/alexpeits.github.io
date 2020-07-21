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

  appSrcRegex = [
    "^app.*$"
    "^peits.cabal$"
    "^LICENSE$"
  ];

  siteSrcRegex = [
    "^posts.*$"
    "^pages.*$"
    "^static.*$"
    "^templates.*$"
    "^config\.yml$"
  ];

  haskellPackages = haskellPackagesBase.override {
    overrides = self: super:
      let
        hsPkgs = import ./nix/overrides.nix {
          pkgs = nixpkgs;
          self = self;
          super = super;
        };
        src = nixpkgs.lib.sourceByRegex ./. appSrcRegex;
        drv = self.callCabal2nix "peits" src {};
      in
        hsPkgs // { peits = drv; };
  };

  deps = [
    nixpkgs.python37Packages.pygments
    nixpkgs.minify
  ];

  site = nixpkgs.stdenv.mkDerivation {
    name = "alexpeits-github-io";
    buildInputs = deps ++ [
      nixpkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    src = nixpkgs.lib.sourceByRegex ./. siteSrcRegex;
    buildPhase = ''
      ${haskellPackages.peits}/bin/peits
    '';
    installPhase = ''
      mkdir "$out"
      cp -R _build "$out/_build"
    '';
  };

  shell = haskellPackages.shellFor {
    packages = ps: [ ps.peits ];
    buildInputs = deps ++ [
      haskellPackages.ghcid
      haskellPackages.cabal-install
      nixpkgs.inotify-tools
    ];
    withHoogle = true;
  };

in

if nixpkgs.lib.inNixShell
then shell
else { site = site; peits = haskellPackages.peits; }
