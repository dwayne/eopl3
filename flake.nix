{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "eopl3";

          packages = [
            pkgs.cabal-install
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-test
            pkgs.haskell.compiler.ghc9103
            pkgs.racket
          ];

          shellHook = ''
            export PROJECT_ROOT="$PWD"
            export PS1="($name)\n$PS1"
          '';
        };
      }
    );
}
