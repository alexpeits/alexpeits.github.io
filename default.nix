# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? null }:

let

  haskellPackages = if isNull compiler then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  hakyll-src = pkgs.fetchFromGitHub {
    owner = "jaspervdj";
    repo = "hakyll";
    rev = "a312fd4972f9add0736a9f8335bcd51e0e163b06";
    sha256 = "0mz5k81rmh5fwj4dflljjhv4c03ygamsb1xncx459rldlma8w1l4";
  };

  alexpeits-github-io = haskellPackages.callPackage ./nix/alexpeits-github-io.nix {
    hakyll = pkgs.haskellPackages.callCabal2nix "hakyll" hakyll-src { };
  };

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
