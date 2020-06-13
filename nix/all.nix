let
  importOrElse = maybePath: otherwise:
    if builtins.pathExists maybePath then import maybePath else otherwise;
  pkgs = importOrElse ./nixpkgs.nix
    ( let
        haskellNix = import (
          builtins.fetchTarball
            https://github.com/lspitzner/haskell.nix/archive/4ad436d66d1a553d1a36d89fcab9329f10ae36e9.tar.gz
        ) { version = 2; };
        nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
      in
      import nixpkgsSrc haskellNix.nixpkgsArgs
    );
  gitignoreSrc = pkgs.fetchFromGitHub {
    # owner = "hercules-ci";
    owner = "lspitzner"; # TODO switch back to the above once PR is merged
                         # see https://github.com/hercules-ci/gitignore.nix/pull/44
    repo = "gitignore.nix";
    rev = "97d53665298d2b31b79e5fe4b60edb12a6661547";
    sha256 = "sha256:1b3z2ikpg32zsfrhv4fb17dqavgg7d4wahslxlm37w68y7adsdav";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource gitignoreFilter;
  cleanedSource = pkgs.lib.cleanSourceWith {
    name = "pqueue";
    src = ./..;
    filter = p: t:
      let baseName = baseNameOf (toString p);
      in gitignoreFilter ./.. p t
      && baseName != ".gitignore"
      && baseName != "nix"
      && baseName != "shell.nix"
      && baseName != "default.nix";
  };
  localExtraDeps = importOrElse ./local-extra-deps.nix (_: []) {inherit pkgs;};
  args = {
    inherit pkgs;
    inherit cleanedSource;
    pkg-def-extras = localExtraDeps;
  };
  inherit (builtins) hasAttr;
in
assert pkgs.lib.assertMsg (hasAttr "haskell-nix" pkgs) "need iohk haskell-nix overlay!";
let
  versions = {
    # "stack-8.0" = import ./via-stack.nix (args // { resolver = "lts-9.21"; });
    # "stack-8.2" = import ./via-stack.nix (args // { resolver = "lts-11.22"; });
    "stackage-8.4" = import ./via-stackage.nix (args // {
      # resolver = "lts-12.26";
      stackFile = "stack-8.4.yaml";
    });
    "stackage-8.6" = import ./via-stackage.nix (args // {
      # resolver = "lts-14.27";
      stackFile = "stack-8.6.yaml";
    });
    "stackage-8.8" = import ./via-stackage.nix (args // {
      # resolver = "lts-15.12";
      stackFile = "stack-8.8.yaml";
    });
    "hackage-8.4" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc844";
      index-state = "2020-05-01T00:00:00Z";
    });
    "hackage-8.6" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc865";
      index-state = "2020-05-01T00:00:00Z";
    });
    "hackage-8.8" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc883";
      index-state = "2020-05-01T00:00:00Z";
    });
  } // (if hasAttr "ghc8101" pkgs.haskell-nix.compiler
    then {
    "hackage-8.10" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc8101";
      index-state = "2020-06-06T00:00:00Z";
    });
    } else builtins.trace "warn: ghc 8.10 is not avaiable, will not be tested!" {}
  );
in
versions // {
  inherit cleanedSource;
  default = versions."stackage-8.8";
}