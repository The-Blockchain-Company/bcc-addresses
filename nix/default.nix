{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  bcccoinNix = import sources.bcccoin-nix {};
  haskellNix = (import sources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs;
  # Use our own nixpkgs if it exists in sources.json.
  # Otherwise, take the pinned version from bcccoin-nix.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using The-Blockchain-Company default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else bcccoinNix.nixpkgs;

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/The-Blockchain-Company/haskell.nix)
    haskellNix.overlays
    # bcccoinNix: nix utilities and niv:
    ++ bcccoinNix.overlays.bcccoinNix

    # Our haskell-nix-ified cabal project overlay:
    ++ [ (import ./pkgs.nix) ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
