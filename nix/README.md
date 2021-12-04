
This project uses the https://github.com/lspitzner/seaaye tool to build via
nix. Please refer to its documentation for detailed help.

For basic usage, running `./nix/cimake.sh shell` should drop you in a nix-shell
to start developing/maintaining. `./nix/cimake.sh ci` will run a full check
against multiple targets, but that _will_ need to compile certain ghcs
that are not available in iohk's binary caches.

Speaking of which, as seaaye is using iohk's "haskell.nix" toolkit, you
probably want to set up the relevant binary caches for your nix installation.

See https://input-output-hk.github.io/haskell.nix/iohk-nix/
