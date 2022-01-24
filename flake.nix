{
  outputs = { self, nixpkgs }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    myghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs;
    [ lens
      generic-lens
      aeson
      http-conduit
      optparse-generic
      turtle
    ]);
  in with pkgs; {
    defaultPackage.x86_64-linux = stdenv.mkDerivation rec {
      ldpath = lib.makeLibraryPath buildInputs;
      buildInputs =
      [ myghc
        gnumake
        patchelf
        zlib
        gmp
        libffi
      ];
      name = "invicli";
      src = self;
      buildPhase = "make";
      installPhase = ''
          mkdir -p $out/bin; install -t $out/bin invicli
          patchelf --set-rpath ${ldpath} $out/bin/invicli
      '';
    };
    devShell.x86_64-linux = mkShell { buildInputs = [ myghc gnumake]; };
  };
}
