# IMPORTANT: You must follow this so you don't build several copies of GHC.
# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started#setting-up-the-binary-cache

# https://docs.haskellstack.org/en/stable/topics/nix_integration/

{
  description = "p3";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { pkgs, ... }:
        let
          # Use GHC 9.8.4 to match Stack LTS 23.0
          hPkgs = pkgs.haskell.packages.ghc98;

          # Wrap Stack to work with our Nix integration. We do not want to modify
          # stack.yaml so non-Nix users do not notice anything.
          # - no-nix: We do not want Stack's way of integrating Nix.
          # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
          # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
          stackWrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
        in
        {
          devShells.default = pkgs.mkShell {
            name = "p3-dev-environment";

            buildInputs = [
              hPkgs.ghc
              # GHC compiler in the desired version (will be available on PATH)
              hPkgs.ghcid # Continuous terminal Haskell compile checker
              hPkgs.ormolu # Haskell formatter
              hPkgs.hlint # Haskell codestyle checker
              hPkgs.hoogle # Lookup Haskell documentation
              hPkgs.haskell-language-server # LSP server for editor
              # hPkgs.cabal-install
              stackWrapped
              pkgs.zlib # External C library needed by some Haskell packages
            ];

            shellHook = ''
              echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
              echo "p3 Haskell Development Environment"
              echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
              echo "Stack version: $(stack --version | head -n1)"
              echo "GHC version:   $(ghc --version)"
              echo ""
              echo "Available commands:"
              echo "  stack build      - Build the project"
              echo "  stack test       - Run tests"
              echo "  stack run sim    - Run simulator"
              echo "  stack run front  - Run frontend parser"
              echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            '';
          };
        };
    };
}
