let
  # This was the latest commit on Nov 15 2023.
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/28189330044e1fe145c55dc9560474121ae21ad9";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  packages = with pkgs; [
    haskell.compiler.ghc963
    haskellPackages.cabal-fmt
    stylish-haskell
    zlib.dev
  ];
}

## Fix whitespace: https://github.com/conal/felix/commit/b3208daa1f6924d79748d5aa64626aa3e65395f8
