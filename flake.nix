{
  description = "Grimoire - custom static site generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            ormolu  # formatter
          ];
          
          shellHook = ''
            echo "Grimoire dev environment"
            echo "GHC version: $(ghc --version)"
            echo "Run 'cabal build' to build"
          '';
        };
      });
}