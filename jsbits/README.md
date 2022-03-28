# GHCJS Build of bcc-addresses

How to build `bcc-addresses` library, executables, and tests with
ghcjs.

## With Nix

Build and run CLI:

```terminal
$ nix-build release.nix -A ghcjs.bcc-address.x86_64-linux
$ node ./result/bin/bcc-address.jsexe/all.js --help
$ node ./result/bin/bcc-address.jsexe/all.js recovery-phrase generate
```

Execute library unit tests:
```terminal
$ nix-build release.nix -A ghcjs.checks.bcc-addresses.unit.x86_64-linux
```

## With Cabal in nix-shell

The `nix-shell` development environment provides
`js-unknown-ghcjs-cabal`, which is a cross-compiling Cabal for ghcjs.

Build and run CLI:

```terminal
$ nix-shell
[nix-shell:~/tbco/bcc-addresses]$ js-unknown-ghcjs-cabal --builddir=dist-ghcjs build all
...

[nix-shell:~/tbco/bcc-addresses]$ js-unknown-ghcjs-cabal --builddir=dist-ghcjs run bcc-addresses-cli:exe:bcc-address
...
/home/rodney/tbco/bcc-addresses/dist-ghcjs/build/wasm32-none/ghcjs-8.6.5/bcc-addresses-cli-3.3.0/x/bcc-address/build/bcc-address/bcc-address: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
bcc-address: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)

[nix-shell:~/tbco/bcc-addresses]$ node dist-ghcjs/build/wasm32-none/ghcjs-8.6.5/bcc-addresses-cli-3.3.0/x/bcc-address/build/bcc-address/bcc-address.jsexe/all.js recovery-phrase generate
culture fringe exercise stumble gold current balance ....
```

### Limitations

1. `js-unknown-ghcjs-cabal run` doesn't work ghcjs code needs to be
   interpreted with `nodejs`.

2. We needed to add dummy calls to `Bcc.Address.Jsbits.addJsbitsDependency`
   to ensure that ghcjs linked in the emscripten-compiled crypto code.

## Without Nix

This is more difficult because you need to manually install correct
versions of build tools and dependencies.

Use the script `jsbits/emscripten/build.sh` to make
`bcc-crypto.js` and then install the `bcc-addresses-jsbits`
library with ghcjs Cabal.
