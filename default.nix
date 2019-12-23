# nixpkgs haskell-updates 2019-12-22
# required commit: https://github.com/NixOS/nixpkgs/pull/75882
{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs/archive/f3293015015a907dc116981eb1ee0edd0410c5f9.tar.gz)
}:
with (import nixpkgs {});
let
  # Latest version of elm2nix doesn't support Elm 0.19.1
  # using master as of 2019-12-23
  # issue: https://github.com/hercules-ci/elm2nix/issues/36
  elm2nix-head = fetchFromGitHub {
    owner = "hercules-ci";
    repo = "elm2nix";
    sha256 = "0ing539di3a3j8ynplmq520256rr9fwmf9acz1z49b1501kxhw56";
    rev = "38b61e45a860d7512f2ec2e458cf01c688835ddf";
  };
in {
  shell = mkShell {
    buildInputs = with elmPackages; [ elm elm-format elm2nix-head ];
  };
  html = import ./nix/nixos-status.nix { inherit nixpkgs; };
}
