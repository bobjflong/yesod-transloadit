with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, byteable, bytestring, cryptohash
             , hspec, lens, lens-aeson, old-locale, shakespeare, stdenv, text
             , time, transformers, yesod, yesod-core, yesod-form, yesod-test
             }:
             mkDerivation {
               pname = "transloadit-yesod";
               version = "0.1.0.0";
               sha256 = "deleteme";
               buildDepends = [
                 aeson base byteable bytestring cryptohash lens lens-aeson
                 old-locale shakespeare text time transformers yesod yesod-core
                 yesod-form
               ];
               testDepends = [
                 base hspec old-locale text time yesod yesod-form yesod-test
               ];
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
