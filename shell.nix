{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cabal-install, stdenv }:
      mkDerivation {
        pname = "OOHaskell-encodings";
        version = "0.1.0.0";
        sha256 = "./.";
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base ];
        buildTools = [ cabal-install ];
        homepage = "http://code.haskell.org/OOHaskell/";
        description = "Various encodings from the OOHaskell paper";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
