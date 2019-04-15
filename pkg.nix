{ mkDerivation, base, bytestring, case-insensitive, network-simple
, safe-exceptions, stdenv, websockets
}:
mkDerivation {
  pname = "network-simple-ws";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive network-simple safe-exceptions
    websockets
  ];
  homepage = "https://github.com/k0001/network-simple-ws";
  description = "Simple interface to WebSockets";
  license = stdenv.lib.licenses.bsd3;
}
