{
  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux;
      mkShell {
        buildInputs = [ (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          aeson
          http-conduit
        ])) ];
      };
  };
}
