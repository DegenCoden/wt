{ mkDerivation, aeson, base, brick, bytestring, directory
, http-client, http-client-tls, optparse-applicative, stdenv
, temporary, text, typed-process, vector, vty
}:
mkDerivation {
  pname = "wt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring directory http-client http-client-tls
    optparse-applicative temporary text typed-process vector vty
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
