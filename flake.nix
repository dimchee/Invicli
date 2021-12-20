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
    devShell.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux;
      mkShell {
        buildInputs = [ (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          relude_1_0_0_1
          aeson
          http-conduit
        ])) ];
      };
  };
}
