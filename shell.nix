{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, byteable, bytestring, containers
      , cryptohash, hspec, lens, lens-aeson, old-locale, shakespeare
      , stdenv, text, time, transformers, unordered-containers, yesod
      , yesod-core, yesod-form, yesod-test
      }:
      mkDerivation {
        pname = "yesod-transloadit";
        version = "0.2.1.0";
        src = ./.;
        buildDepends = [
          aeson base byteable bytestring cryptohash lens lens-aeson
          old-locale shakespeare text time transformers unordered-containers
          yesod yesod-core yesod-form
        ];
        testDepends = [
          aeson base containers hspec lens old-locale text time yesod
          yesod-form yesod-test
        ];
        description = "Transloadit support for Yesod";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
