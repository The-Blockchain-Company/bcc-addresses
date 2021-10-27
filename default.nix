{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and bcccoin-nix:
# nix build -f default.nix bcc-address --arg sourcesOverride '{
#   bcccoin-nix = ../bcccoin-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (bcccoin-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.bcccoinNix.commitIdFromGitRepoOrZero ./.git
}:
let
  inherit (pkgs.haskell-nix.haskellLib)
    selectProjectPackages
    collectComponents'
    collectChecks';

  haskellPackages = pkgs.lib.dontRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages pkgs.bccAddressesHaskellPackages);

  self = {
    inherit haskellPackages pkgs;
    inherit (haskellPackages.bcc-addresses-cli.components.exes) bcc-address;
    inherit (haskellPackages.bcc-addresses.components) library;

    # The built (but not executed) test suites.
    tests = collectComponents' "tests" haskellPackages;
    # The results of executing the tests.
    checks = collectChecks' haskellPackages;

    # Development environment with bonus ghcjs cross-compiler.
    shell = pkgs.bccAddressesHaskellPackages.shellFor {
      crossPlatforms = p: [ p.ghcjs ];
      tools.cabal = {};
      tools.hpack = {};
      buildInputs = [ pkgs.nodejs ];
      packages = ps: [ ps.bcc-addresses ps.bcc-addresses-cli ps.bcc-addresses-jsapi  ];
    };
  };
in
  self
