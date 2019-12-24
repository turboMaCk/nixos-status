# nixpkgs haskell-updates 2019-12-22
# required commit: https://github.com/NixOS/nixpkgs/pull/75882
{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs/archive/f3293015015a907dc116981eb1ee0edd0410c5f9.tar.gz)
}:
let
  pkgs = import nixpkgs {};
in with pkgs;
let
  # Latest version of elm2nix doesn't support Elm 0.19.1
  # using master as of 2019-12-23
  # issue: https://github.com/hercules-ci/elm2nix/issues/36
  elm2nix-master = import (fetchFromGitHub {
    owner = "hercules-ci";
    repo = "elm2nix";
    sha256 = "0ing539di3a3j8ynplmq520256rr9fwmf9acz1z49b1501kxhw56";
    rev = "38b61e45a860d7512f2ec2e458cf01c688835ddf";
  }) { inherit pkgs; };
in rec {
  shell = mkShell {
    buildInputs = with elmPackages; [
      elm
      elm-format
      elm2nix-master
    ];
  };

  elm-app = import ./nix/nixos-status.nix { inherit nixpkgs; };

  www = stdenv.mkDerivation {
    name = "nixos-status";
    version = "0.1.0";

    src = ./assets;

    installPhase = ''
      mkdir -p $out/www/assets
      cp ${elm-app}/Main.html $out/www/index.html
      cp nixos-logo.svg $out/www/assets/
    '';
  };
}
