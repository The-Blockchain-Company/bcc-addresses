// Copyright © 2021 The-Blockchain-Company
// License: Apache-2.0

/**
 * Bcc-addresses types.
 *
 * @module
 */

/**
 * Hex-encoded bytes.
 *
 * _TODO:_ what's the best way of representing bytestrings in
 * TypeScript?
 */
export type Bytes = string;
/** Verification key hash. */
export type KeyHash = Bytes;
/** Hash of a script. */
export type ScriptHash = Bytes;

/**
 * A Bcc address, encoded as bech32, base58, or hexadecimal.
 */
export type Address = string;

/**
 * A bech32-encoded extended public key.
 *
 * _TODO:_ Add proper XPub type, which is the result of bech32 parsing
 * a string.
 */
export type XPub = string;

/** Supported address formats for the Bcc Sophie era. */
export type AddressStyle = "Sophie" | "Icarus" | "Cole";

/** How the stake at this address will be delegated. */
export type StakeReference
  /** No delegation. */
  = "none"
  /** Stake key is in address. */
  | "by value"
  /** Look up certificate in transaction at slot. */
  | "by pointer";

/**
 * The return value of [[inspectAddress]].
 */
export type InspectAddress
    = InspectAddressSophie
    | InspectAddressIcarus
    | InspectAddressCole;

/**
 * A [[StakeReference | stake reference]] pointer.
 */
export interface ChainPointer {
  slot_num: number;
  transaction_index: number;
  output_index: number;
};

export interface InspectAddressSophie {
  address_style: "Sophie";
  /** An integer denoting which network the address belongs to. */
  network_tag: number;
  spending_key_hash?: KeyHash;
  spending_key_hash_bech32?: string;
  stake_reference?: StakeReference;
  pointer?: ChainPointer;
  stake_key_hash?: KeyHash;
  stake_key_hash_bech32?: string;
  script_hash?: ScriptHash;
  script_hash_bech32?: string;
};

/**
 * Corresponds to `Bcc.Address.Style.Icarus.AddressInfo`.
 */
export interface InspectAddressIcarus {
  address_style: "Icarus";
  /** Which network the address belongs to. Unset for mainnet. */
  network_tag: number;
  /** Hex-encoded address payload */
  address_root: Bytes;
};

/**
 * Corresponds to `Bcc.Address.Style.Cole.AddressInfo`.
 */
export interface InspectAddressCole {
  address_style: "Cole";
  /** Which network the address belongs to. Unset for mainnet. */
  network_tag?: number;
  /** Hex-encoded address payload */
  address_root: Bytes;
  /** Heirarchical derivation payload. If a root XPub is provided,
      the derivation indices are decrypted.
      Otherwise, it will be the encrypted payload. **/
  payload: Bytes  | { account_index: number; address_index: number; };
}

/**
 * Represents a failure to decode the given address.
 */
export interface ErrInspectAddress {
  error: {
    code: string;
    details?: unknown;
  };
  message: string;
};
