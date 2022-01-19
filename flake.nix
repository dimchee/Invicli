{
  outputs = { self, nixpkgs }: {
    # devShell.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux;
    #   haskellPackages.developPackage {
    #     root = ./.;
    #     modifier = drv:
    #     haskell.lib.addBuildTools drv (with haskellPackages;
    #     [ cabal-install
    #       ghcid
    #     ]);
    #   };

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation rec {
        ldpath = lib.makeLibraryPath buildInputs;
        buildInputs = [ (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          lens
          generic-lens
          aeson
          http-conduit
          optparse-generic
          turtle
        ]))
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
    devShell.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux;
      mkShell {
        buildInputs = [ (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          lens
          generic-lens
          aeson
          http-conduit
          optparse-generic
          turtle
        ])) ];
      };
  };
}
