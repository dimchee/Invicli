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
          lens
          generic-lens
          aeson
          http-conduit
        ])) ];
      };
  };
}
