{ mkDerivation, base, hakyll, stdenv, lib }:
mkDerivation {
  pname = "alexpeits-github-io";
  version = "0.1.0.0";
  src = lib.sourceByRegex ../. [
    "alexpeits-github-io.cabal"
    "site.hs"
  ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
