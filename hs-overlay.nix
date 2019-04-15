{ pkgs }:

self: super: {
  network-simple-ws = super.callPackage ./pkg.nix {};
  network-simple =
    super.callPackage
      ({ mkDerivation, base, bytestring, network, safe-exceptions, socks
       , stdenv, transformers
       }:
       mkDerivation {
         pname = "network-simple";
         version = "0.4.3";
         sha256 = "0dd5cf1ed308bbe9601dc39026419151f552f386ec5e82417ad4f86cc4539028";
         revision = "1";
         editedCabalFile = "1xyz4b24vgnidvd43cfmf0k6090dayhfcp6n8x894ibd2mq3vash";
         libraryHaskellDepends = [
           base bytestring network safe-exceptions socks transformers
         ];
         homepage = "https://github.com/k0001/network-simple";
         description = "Simple network sockets usage patterns";
         license = stdenv.lib.licenses.bsd3;
       }
      )
      {};
}
