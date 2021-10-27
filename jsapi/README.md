# bcc-addresses TypeScript binding

- 📦 [NPM Package](https://www.npmjs.com/package/bcc-addresses)
- 📗 [API Documentation](https://The-Blockchain-Company.github.io/bcc-addresses/typescript/)
- 🎇 [Web Demo](https://The-Blockchain-Company.github.io/bcc-addresses/demo/)
- λ [Haskell Library](https://The-Blockchain-Company.github.io/bcc-addresses/haddock/)

This is a Typescript (or Javascript) version of the
[`bcc-addresses`](https://github.com/The-Blockchain-Company/bcc-addresses)
API.

For the time being, this module is **experimental**, and exposes only
a subset of the functionality of the Haskell API. We would like to
increase the TypeScript binding's coverage of the Haskell API in
future, as needs arise.

## How it works

This package comprises of four parts:

 * A [GHCJS](https://github.com/ghcjs/ghcjs) build of the
   [`bcc-addresses`](../core/bcc-addresses.cabal) library.

 * An [emscripten](https://emscripten.org/) build of the
   [libsodium](https://github.com/The-Blockchain-Company/libsodium) crypto library,
   supported by the [`bcc-addresses-jsbits`](../jsbits/bcc-addresses-jsbits.cabal) package.

 * A Cabal package [`bcc-addresses-jsapi`](./bcc-addresses-jsapi.cabal)
   containing GHCJS foreign exports for translating Javascript
   function calls and values into Haskell function calls and values,
   and vice-versa.

 * An NPM package [`bcc-addresses`](./package.json) containing both
   CommonJS and EcmaScript modules written in TypeScript, which thinly
   wraps the GHCJS foreign exports in order to make a proper API.

## Development Info

### NodeJS module: Building and testing

Start a `nix-shell` in *this directory* (not the top-level) and run:

```shell
$ cd jsapi
$ nix-shell
[nix-shell:~/bcccoin/bcc-addresses/jsapi]$ npm install && npm run build
...
[nix-shell:~/bcccoin/bcc-addresses/jsapi]$ npm run test
```

Behind the scenes, this will use Nix to make the `ghcjs` build of the `bcc-addresses` library. The path to this Javascript file is stored in the `$BCC_ADDRESSES_JS` environment variable.

### Haskell module: Building and testing

To try it out run `nix-shell` from the repo top-level directory:

```shell
$ nix-shell
[nix-shell:~/bcccoin/bcc-addresses]$ js-unknown-ghcjs-cabal build bcc-addresses-jsapi:jsapi-test
[nix-shell:~/bcccoin/bcc-addresses]$ node dist-newstyle/build/js-ghcjs/ghcjs-8.10.4/bcc-addresses-jsapi-3.5.0/t/jsapi-test/build/jsapi-test/jsapi-test.jsexe/all.js
```

That test initializes the api from a JS function that is called from `Main.hs`. To build `.js` file that might be easier to use from a JS app run:

```shell
$ nix-build jsapi/default.nix -A bcc-addresses-js
/nix/store/dw0xwvjvwac68i2a4dkkpx4mw8yji9z8-bcc-addresses-js-3.5.0
$ tree ./result
./result
├── bcc-addresses-jsapi.cjs.js
└── bcc-addresses-jsapi.js
```

This replaces the regular `runmain.js` with `jsapi/glue/runmain.js`. That exposes a single function you can call and pass in a continuation.

To initialize, call the `runBccAddressesApi` with a continuation that like the one in `jsapi/glue/test.js`. You will be passed `api` and `cleanup`.

## TODO

- [ ] More API endpoints depending on user needs.
- [ ] Used "typed" objects as parameters for the `inspectAddress` API, instead of strings which must be parsed.
- [ ] Add a build step to optimise output file sizes (i.e. minification, tree shaking, etc).
- [ ] Solve issue on `nodejs` where registered event handlers remain after API cleanup, preventing the `nodejs` runtime from exiting.
- [ ] Add helper functions to the JSaddle API so that it can output code for ES6 Promises.
- [ ] Bring back headless testing of JSaddle code.
- [ ] Replace TSDX with a better build system.
- [ ] Automatically update docs in CI.
- [ ] Add release workflow to CI which builds the package and uploads to NPM.

## More details

### NodeJS package

For better or worse, this is based on the
[TSDX](https://github.com/palmerhq/tsdx) template. The TSDX standard
documentation follows...

#### Commands

To run TSDX, use:

```bash
npm start # or yarn start
```

This builds to `/dist` and runs the project in watch mode so any edits you save inside `src` causes a rebuild to `/dist`.

To do a one-off build, use `npm run build` or `yarn build`.

To run tests, use `npm test` or `yarn test`.

#### Configuration

Code quality is set up for you with `prettier`, and `lint-staged`. Adjust the respective fields in `package.json` accordingly.

##### Jest

Jest tests are set up to run with `npm test` or `yarn test`.

##### Bundle Analysis

[`size-limit`](https://github.com/ai/size-limit) is set up to calculate the real cost of your library with `npm run size` and visualize the bundle with `npm run analyze`.

##### Rollup

TSDX uses [Rollup](https://rollupjs.org) as a bundler and generates multiple rollup configs for various module formats and build settings. See [Optimizations](#optimizations) for details.

##### TypeScript

`tsconfig.json` is set up to interpret `dom` and `esnext` types, as well as `react` for `jsx`. Adjust according to your needs.

#### Continuous Integration

### GitHub Actions

Two actions are added by default:

- `tsdx-build` which installs deps w/ cache, lints, tests, and builds on all pushes against a Node and OS matrix
- `tsdx-size` which comments cost comparison of your library on every pull request using [`size-limit`](https://github.com/ai/size-limit)

#### Optimizations

Please see the main `tsdx` [optimizations docs](https://github.com/palmerhq/tsdx#optimizations). In particular, know that you can take advantage of development-only optimizations:

```js
// ./types/index.d.ts
declare var __DEV__: boolean;

// inside your code...
if (__DEV__) {
  console.log('foo');
}
```

You can also choose to install and use [invariant](https://github.com/palmerhq/tsdx#invariant) and [warning](https://github.com/palmerhq/tsdx#warning) functions.

#### Module Formats

CJS, ESModules, and UMD module formats are supported.

The appropriate paths are configured in `package.json` and `dist/index.js` accordingly. Please report if any issues are found.

#### Publishing to NPM

TSDX recommend using [np](https://github.com/sindresorhus/np).
