
#nix-shell -p zlib haskellPackages.ghc
# References: 
# https://scrive.github.io/nix-workshop/01-getting-started/01-introduction.html
# 

with (import <nixpkgs> { });
mkShell {
 buildInputs = [
   zlib
   haskellPackages.ghc
   haskellPackages.ghcid
   haskellPackages.ghcide
  ];
}
