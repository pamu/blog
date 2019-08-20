{ mkDerivation, base, filepath, hakyll, stdenv, time }:
mkDerivation {
  pname = "blog";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath hakyll time ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
