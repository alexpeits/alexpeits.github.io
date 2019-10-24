# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? null }:

let

  haskellPackages = if isNull compiler then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  alexpeits-github-io = haskellPackages.callPackage ./nix/alexpeits-github-io.nix { };

  shell = pkgs.mkShell {
    inputsFrom = [ alexpeits-github-io.env ];
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="make ghcid"
    '';
    buildInputs = [
      haskellPackages.ghcid
    ];
  };

  site = pkgs.callPackage ./nix/site.nix { alexpeits-github-io = alexpeits-github-io; };

in {
  alexpeits-github-io = alexpeits-github-io;
  site = site;
  shell = shell;
}
