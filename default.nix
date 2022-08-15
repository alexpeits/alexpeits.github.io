{ pkgs ? null, compiler ? null }:
let
  nixpkgs =
    if isNull pkgs then
      import (import ./nix/sources.nix).nixos { }
    else if builtins.typeOf pkgs == "set" then
      pkgs
    else
      import (builtins.getAttr pkgs (import ./nix/sources.nix)) { };

  haskellPackagesBase =
    if isNull compiler then
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
    "^citations.*$"
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
        drv = self.callCabal2nix "peits" src { };
      in
      hsPkgs // { peits = drv; };
  };

  isMac =
    builtins.any
      (arch: builtins.currentSystem == arch)
      [ "x86_64-darwin" ];

  inotify = if isMac then [ nixpkgs.entr ] else [ nixpkgs.inotify-tools ];

  nodejs = nixpkgs.nodejs-16_x;
  mermaid-cli = nixpkgs.nodePackages.mermaid-cli.override { nodejs = nodejs; };
  yarn = nixpkgs.yarn.override { nodejs = nodejs; };

  deps = [
    nixpkgs.python310Packages.pygments
    nixpkgs.minify
    # mermaid-cli
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
    buildInputs = deps ++ inotify ++ [
      haskellPackages.ghcid
      haskellPackages.cabal-install
      haskellPackages.ormolu
      nodejs
      yarn
    ];
    withHoogle = true;
  };

in
if nixpkgs.lib.inNixShell
then shell
else { site = site; peits = haskellPackages.peits; }
