let 
  defaultSources = import ./nix/sources.nix;
  defaultNixpkgs = import defaultSources.nixpkgs {};
in

{ nixpkgs ? defaultNixpkgs, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  generator = haskellPackages.callCabal2nix "munihac" (./.) { };

in if pkgs.lib.inNixShell then generator.env else generator
