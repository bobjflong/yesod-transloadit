{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, byteable, bytestring, containers
      , cryptohash, hspec, jmacro, lens, lens-aeson, old-locale
      , shakespeare, stdenv, text, time, transformers
      , unordered-containers, yesod, yesod-core, yesod-form, yesod-test
      }:
      mkDerivation {
        pname = "yesod-transloadit";
        version = "0.4.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base byteable bytestring cryptohash jmacro lens lens-aeson
          old-locale shakespeare text time transformers unordered-containers
          yesod yesod-core yesod-form
        ];
        testHaskellDepends = [
          aeson base containers hspec lens old-locale text time yesod
          yesod-form yesod-test
        ];
        description = "Transloadit support for Yesod";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
