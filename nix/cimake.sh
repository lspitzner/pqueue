#!/bin/bash

set -e

###############################
# START OF PER-PROJECT SETTINGS
###############################
SEAAYE_VERSION=02d9df90256974d5a0fb9adfcb5dcfd25ce6c709
export NIX_CONFIG=$(cat \
<<EOF
{ package-name = "pqueue";
  targets =
  { 
    hackage-8-06 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc865";
    };
    hackage-8-08 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc884";
    };
    hackage-8-10 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc8107";
    };
    hackage-9-01 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc901";
    };
    stackage-8-06 = {
      resolver = "stackage";
      stackFile = "stack-8.6.yaml";
      ghc-ver = "ghc865";
    };
    stackage-8-08 = {
      resolver = "stackage";
      stackFile = "stack-8.8.yaml";
      ghc-ver = "ghc884";
    };
    stackage-8-10 = {
      resolver = "stackage";
      stackFile = "stack-8.10.yaml";
      ghc-ver = "ghc8107";
    };
    stackage-9-01 = {
      resolver = "stackage";
      stackFile = "stack-9.0.yaml";
      ghc-ver = "ghc901";
    };
  };
  module-flags = [
    # N.B.: There are haskell-nix module options. See the haskell-nix docs
    #       for details. Also, be careful about typos: In many cases you
    #       will not get errors but the typo'd flag will just not have any
    #       effect!
    { packages.my-package.flags.my-package-examples-examples = true; }
  ];
  default-target = "hackage-8-06";
  do-check-hackage = "hackage.haskell.org";
  do-check-changelog = "CHANGELOG.md";
  # local-config-path = ./nix/local-config.nix;
}
EOF
)
###############################
# END OF PER-PROJECT SETTINGS #
###############################

# only touch things below if you know what you are doing

SEAAYE_SOURCE="https://github.com/lspitzner/seaaye/archive/$SEAAYE_VERSION.tar.gz"
SEAAYE_STORE=$(nix-instantiate --expr "builtins.fetchTarball $SEAAYE_SOURCE" --eval --json | jq -r)
nix-store -r "$SEAAYE_STORE" --indirect --add-root nix/seaaye >/dev/null
export SEAAYE_LOCAL_CONFIG_PATH=$(nix-instantiate --eval --strict -E "$NIX_CONFIG" -A local-config-path 2>/dev/null)
export SEAAYE_INVOKER_PATH="$0"
export SEAAYE_MAKEFILE=$(realpath nix/seaaye/Makefile)

case "$@" in
  repl|nix-repl|"nix repl")
    nix repl --arg base-config "$NIX_CONFIG" nix/seaaye/main.nix
    ;;
  *)
    make -f nix/seaaye/Makefile "$@"
    ;;
esac
