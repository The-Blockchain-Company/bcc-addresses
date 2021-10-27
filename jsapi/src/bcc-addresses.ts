/// <reference path="./foreign.d.ts" />

// Copyright © 2021 The-Blockchain-Company
// License: Apache-2.0

/**
 * Typescript bindings for `Bcc.Address`.
 *
 * @module
 */

import { Address, XPub, InspectAddress } from './types';
import { BccAddressesJSModule, BccAddressesApi } from './foreign';

/**
 * Get information about a Bcc address. Three address formats are
 * supported: Sophie, Icarus, and Cole.
 *
 * If the address can't be parsed, the promise will be rejected with
 * [[ErrInspectAddress]].
 *
 * @param address the address to inspect.
 * @param rootXPub an optional bech32-encoded root extended public
 *   key. This only applies to Cole addresses, and is for decrypting
 *   the {@link InspectAddressCole.payload} field.
 * @returns The fields parsed from the address.
 */
export async function inspectAddress(address: Address, rootXPub?: XPub): Promise<InspectAddress> {
  const api = await init();
  return new Promise((resolve, reject) =>
    api.inspectAddress(rootXPub || null, address, resolve, reject));
}

/**
 * @returns The Cabal package version string and git revision.
 */
export async function version(): Promise<string> {
  const api = await init();
  return new Promise(done => api.version(done));
}

var apiCreate: undefined|Promise<BccAddressesApi>;
var apiDestroy: undefined|(() => void) = () => {};

/**
 * Start the bcc-addresses runtime system.
 *
 * There is no need to call this because it's done automatically the
 * first time a library function is used.
 *
 * @internal
 */
export function init(): Promise<BccAddressesApi> {
  if (!apiCreate) {
    apiCreate = loadJSAPI().then(mod => {
      const run = mod.runBccAddressesApi;
      return new Promise(resolve => run((api, cleanup) => {
        apiDestroy = cleanup;
        resolve(api);
      }));
    });
  }
  return apiCreate;
}

async function loadJSAPI(): Promise<BccAddressesJSModule> {
  const haveProcess = typeof process != 'undefined';
  const nixPath = haveProcess && process.env?.BCC_ADDRESSES_JS;
  const isNode = haveProcess && process.versions?.node !== 'undefined';
  const mod = isNode ? 'cjs' : 'esm';
  const dir = nixPath || '.';
  const file = `${dir}/bcc-addresses-jsapi.${mod}.js`;
  try {
    return await import(file);
  } catch (err) {
    console.warn(`${file}: ES module loading failed!`, err);
    console.info("Using ambient definitions from bcc-addresses-jsapi.js");
    return { runBccAddressesApi };
  }
}

/**
 * De-allocates resources used for the runtime system.
 *
 * @internal
 */
export function cleanup() {
  if (apiDestroy) {
    apiDestroy();
  }
  apiCreate = undefined;
  apiDestroy = undefined;
}
