# nixpkgs haskell-updates 2019-12-22
# required commit: https://github.com/NixOS/nixpkgs/pull/75882
{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs/archive/f3293015015a907dc116981eb1ee0edd0410c5f9.tar.gz)
}:
with (import nixpkgs {});
mkShell {
  buildInputs = [ elmPackages.elm elm2nix ];
}
