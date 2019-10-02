
let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in stdenv.mkDerivation {
  name = "LispyEnv";
  buildInputs = with pkgs; [
    ghc
    cabal-install
    haskellPackages.ghcid
  ];
  shellHook = ''
   alias repl="ghcid --command=cabal v2-repl"
  '';
}
