{ mkDerivation, aeson, base, bytestring, servant, servant-auth
, servant-server, stdenv, text, utc, wai, warp
}:
mkDerivation {
  pname = "ccchan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring servant servant-auth servant-server text utc
    wai warp
  ];
  homepage = "https://chan.chaoscomputer.club";
  description = "Chaos Computer Chan, the CCC's official image board";
  license = stdenv.lib.licenses.agpl3;
}
