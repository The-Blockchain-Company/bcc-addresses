{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2021 The-Blockchain-Company
-- License: Apache-2.0

module Bcc.Address.Style.Sophie
    ( -- $overview

      -- * Sophie
      Sophie
    , getKey
    , Role (..)
    , roleFromIndex
    , roleToIndex
    , Credential (..)
    , CredentialType (..)

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveDelegationPrivateKey
    , deriveAddressPublicKey

      -- * Addresses
      -- $addresses
    , InspectAddress (..)
    , AddressInfo (..)
    , eitherInspectAddress
    , inspectAddress
    , inspectSophieAddress
    , paymentAddress
    , delegationAddress
    , pointerAddress
    , stakeAddress
    , extendAddress
    , ErrExtendAddress (..)
    , ErrInspectAddressOnlySophie (..)
    , ErrInspectAddress (..)
    , prettyErrInspectAddressOnlySophie
    , prettyErrInspectAddress

      -- * Network Discrimination
    , MkNetworkDiscriminantError (..)
    , mkNetworkDiscriminant
    , inspectNetworkDiscriminant
    , sophieMainnet
    , sophieTestnet

      -- * Unsafe
    , liftXPrv
    , liftXPub
    , unsafeFromRight

      -- Internals
    , minSeedLengthBytes
    , genMasterKeyFromMnemonicSophie
    , deriveAccountPrivateKeySophie
    , deriveAddressPrivateKeySophie
    , deriveAddressPublicKeySophie
    ) where

import Prelude

import Bcc.Address
    ( Address (..)
    , AddressDiscrimination (..)
    , ChainPointer (..)
    , NetworkDiscriminant (..)
    , NetworkTag (..)
    , invariantNetworkTag
    , invariantSize
    , unsafeMkAddress
    )
import Bcc.Address.Derivation
    ( Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index (..)
    , XPrv
    , XPub
    , credentialHashSize
    , deriveXPrv
    , deriveXPub
    , generateNew
    , hashCredential
    , indexFromWord32
    , unsafeMkIndex
    , xpubPublicKey
    )
import Bcc.Address.Internal
    ( WithErrorMessage (..), orElse )
import Bcc.Address.Script
    ( ScriptHash (..) )
import Bcc.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( Alternative )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( Exception, displayException )
import Control.Exception.Base
    ( assert )
import Control.Monad
    ( unless, when )
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.Aeson
    ( ToJSON (..), (.=) )
import Data.Bifunctor
    ( bimap, first )
import Data.Binary.Get
    ( runGetOrFail )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe, isNothing )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word8 )
import Data.Word7
    ( getVariableLengthNat, putVariableLengthNat )
import Fmt
    ( Buildable, build, format, (+|), (|+) )
import GHC.Generics
    ( Generic )

import qualified Bcc.Address.Derivation as Internal
import qualified Bcc.Address.Style.Cole as Cole
import qualified Bcc.Address.Style.Icarus as Icarus
import qualified Bcc.Codec.Bech32.Prefixes as CIP5
import qualified Data.Aeson as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Bcc.Address.Derivation.GenMasterKey': for generating Sophie master keys from mnemonic sentences
-- - 'Bcc.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Bcc.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
--
-- - 'paymentAddress': for constructing payment addresses from a address public key or a script
-- - 'delegationAddress': for constructing delegation addresses from payment credential (public key or script) and stake credential (public key or script)
-- - 'pointerAddress': for constructing delegation addresses from payment credential (public key or script) and chain pointer
-- - 'stakeAddress': for constructing reward accounts from stake credential (public key or script)

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Sophie 'RootK XPrv
-- let accountPubKey  = Sophie 'AccountK XPub
-- let addressPubKey  = Sophie 'PaymentK XPub
-- @
--
-- @since 2.0.0
newtype Sophie (depth :: Depth) key = Sophie
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Sophie depth))
instance (NFData key) => NFData (Sophie depth key)

-- | Describe what the keys within an account are used for.
--
-- - UTxOExternal: used for public addresses sent to other parties for receiving money.
-- - UTxOInternal: generated by wallet software to send change back to the wallet.
-- - Stake: used for stake key(s) and delegation.
--
-- @since 3.0.0
data Role
    = UTxOExternal
    | UTxOInternal
    | Stake
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData Role

roleFromIndex :: Index 'Soft depth -> Maybe Role
roleFromIndex ix = case indexToWord32 ix of
    0 -> Just UTxOExternal
    1 -> Just UTxOInternal
    2 -> Just Stake
    _ -> Nothing

roleToIndex :: Role -> Index 'Soft depth
roleToIndex = unsafeMkIndex . \case
    UTxOExternal -> 0
    UTxOInternal -> 1
    Stake -> 2

--
-- Key Derivation
--
-- $keyDerivation
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Bcc.Mnemonic ( mkSomeMnemonic )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Sophie 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- > let Just accIx = indexFromWord32 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let Just addIx = indexFromWord32 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- > let stakeK = deriveDelegationPrivateKey acctK

instance Internal.GenMasterKey Sophie where
    type SecondFactor Sophie = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Sophie $ genMasterKeyFromMnemonicSophie fstFactor sndFactor

instance Internal.HardDerivation Sophie where
    type AccountIndexDerivationType Sophie = 'Hardened
    type AddressIndexDerivationType Sophie = 'Soft
    type WithRole Sophie = Role

    deriveAccountPrivateKey (Sophie rootXPrv) accIx =
        Sophie $ deriveAccountPrivateKeySophie rootXPrv accIx purposeIndex

    deriveAddressPrivateKey (Sophie accXPrv) role addrIx =
        Sophie $ deriveAddressPrivateKeySophie accXPrv role addrIx

instance Internal.SoftDerivation Sophie where
    deriveAddressPublicKey (Sophie accXPub) role addrIx =
        Sophie $ deriveAddressPublicKeySophie accXPub role addrIx

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 2.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Sophie 'RootK XPrv
genMasterKeyFromMnemonic =
    Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 2.0.0
genMasterKeyFromXPrv
    :: XPrv -> Sophie 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv

-- Re-export from 'Bcc.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 2.0.0
deriveAccountPrivateKey
    :: Sophie 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Sophie 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey

-- Re-export from 'Bcc.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 2.0.0
deriveAddressPrivateKey
    :: Sophie 'AccountK XPrv
    -> Role
    -> Index 'Soft 'PaymentK
    -> Sophie 'PaymentK XPrv
deriveAddressPrivateKey =
    Internal.deriveAddressPrivateKey

-- Re-export from 'Bcc.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives an address public key from the given account public key.
--
-- @since 2.0.0
deriveAddressPublicKey
    :: Sophie 'AccountK XPub
    -> Role
    -> Index 'Soft 'PaymentK
    -> Sophie 'PaymentK XPub
deriveAddressPublicKey =
    Internal.deriveAddressPublicKey

-- Re-export from 'Bcc.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derive a delegation key for a corresponding 'AccountK'. Note that wallet
-- software are by convention only using one delegation key per account, and always
-- the first account (with index 0').
--
-- Deriving delegation keys for something else than the initial account is not
-- recommended and can lead to incompatibility with existing wallet softwares
-- (Klarity, klarity2, Bcclite...).
--
-- @since 2.0.0
deriveDelegationPrivateKey
    :: Sophie 'AccountK XPrv
    -> Sophie 'DelegationK XPrv
deriveDelegationPrivateKey accXPrv =
    let (Sophie stakeXPrv) =
            deriveAddressPrivateKey accXPrv Stake (minBound @(Index 'Soft _))
    in Sophie stakeXPrv

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress' from public key credential
--
-- > import Bcc.Address ( bech32 )
-- > import Bcc.Address.Derivation ( toXPub )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let paymentCredential = PaymentFromKey $ (toXPub <$> addrK)
-- > bech32 $ paymentAddress tag paymentCredential
-- > "addr1vxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdncxsce5t"
--
-- === Generating a 'PaymentAddress' from script credential
--
-- > import Bcc.Address.Script.Parser ( scriptFromString )
-- > import Bcc.Address.Script ( toScriptHash )
-- > import Codec.Binary.Encoding ( encode )
-- > import Data.Text.Encoding ( decodeUtf8 )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let verKey1 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"
-- > let verKey2 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"
-- > let scriptStr = "all [" ++ verKey1 ++ ", " ++ verKey2 ++ "]"
-- > let (Right script) = scriptFromString scriptStr
-- > let infoScriptHash@(ScriptHash bytes) = toScriptHash script
-- > decodeUtf8 (encode EBase16 bytes)
-- > "a015ae61075e25c3d9250bdcbc35c6557272127927ecf2a2d716e29f"
-- > bech32 $ paymentAddress tag (PaymentFromScript infoScriptHash)
-- > "addr1wxspttnpqa0zts7ey59ae0p4ce2hyusj0yn7eu4z6utw98c9uxm83"
--
-- === Generating a 'DelegationAddress'
--
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let paymentCredential = PaymentFromKey $ (toXPub <$> addrK)
-- > let delegationCredential = DelegationFromKey $ (toXPub <$> stakeK)
-- > bech32 $ delegationAddress tag paymentCredential delegationCredential
-- > "addr1qxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdn7nudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyqmkc5xa"
--
-- === Generating a 'PointerAddress'
--
-- > import Bcc.Address ( ChainPointer (..) )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let ptr = ChainPointer 123 1 2
-- > let paymentCredential = PaymentFromKey $ (toXPub <$> addrK)
-- > bech32 $ pointerAddress tag paymentCredential ptr
-- > "addr1gxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdnmmqypqfcp5um"
--
-- === Generating a 'DelegationAddress' from using the same script credential in both payment and delegation
-- > bech32 $ delegationAddress tag (PaymentFromScript infoScriptHash) (DelegationFromScript infoScriptHash)
-- > "addr1xxspttnpqa0zts7ey59ae0p4ce2hyusj0yn7eu4z6utw98aqzkhxzp67yhpajfgtmj7rt3j4wfepy7f8ane294cku20swucnrl"

-- | Possible errors from inspecting a Sophie, Icarus, or Cole address.
--
-- @since 3.4.0
data ErrInspectAddress
    = WrongInputSize Int -- ^ Unexpected size
    | ErrSophie ErrInspectAddressOnlySophie
    | ErrIcarus Icarus.ErrInspectAddress
    | ErrCole Cole.ErrInspectAddress
    deriving (Generic, Show, Eq)
    deriving ToJSON via WithErrorMessage ErrInspectAddress

instance Exception ErrInspectAddress where
    displayException = prettyErrInspectAddress

-- | Possible errors from inspecting a Sophie address
--
-- @since 3.4.0
data ErrInspectAddressOnlySophie
    = PtrRetrieveError String -- ^ Human readable error of underlying operation
    | UnknownType Word8 -- ^ Unknown value in address type field
    deriving (Generic, Eq, Show)
    deriving ToJSON via WithErrorMessage ErrInspectAddressOnlySophie

instance Exception ErrInspectAddressOnlySophie where
    displayException = prettyErrInspectAddressOnlySophie

-- | Pretty-print an 'ErrInspectAddressOnlySophie'
--
-- @since 3.4.0
prettyErrInspectAddressOnlySophie :: ErrInspectAddressOnlySophie -> String
prettyErrInspectAddressOnlySophie = \case
    PtrRetrieveError s ->
        format "Failed to retrieve pointer (underlying errors was: {})" s
    UnknownType t ->
        format "Unknown address type {}" t

-- | Pretty-print an 'ErrInspectAddress'
--
-- @since 3.0.0
prettyErrInspectAddress :: ErrInspectAddress -> String
prettyErrInspectAddress = \case
    WrongInputSize i -> format "Wrong input size of {}" i
    ErrSophie e -> "Invalid Sophie address: "
        <> prettyErrInspectAddressOnlySophie e
    ErrIcarus e -> "Invalid Icarus address: "
        <> Icarus.prettyErrInspectAddress e
    ErrCole e -> "Invalid Cole address: "
        <> Cole.prettyErrInspectAddress e

-- Determines whether an 'Address' a Sophie address.
--
-- Throws 'AddrError' if it's not a valid Sophie address, or a ready-to-print
-- string giving details about the 'Address'.
--
-- @since 2.0.0
inspectSophieAddress
    :: (Alternative m, MonadThrow m)
    => Maybe XPub
    -> Address
    -> m Json.Value
inspectSophieAddress = inspectAddress
{-# DEPRECATED inspectSophieAddress "use qualified 'inspectAddress' instead." #-}

-- | Analyze an 'Address' to know whether it's a valid address for the Bcc
-- Sophie era. Sophie format addresses, as well as old-style Cole and Icarus
-- addresses can be parsed by this function.
--
-- Returns a JSON value containing details about the 'Address', or throws
-- 'ErrInspectAddress' if it's not a valid address.
--
-- @since 3.0.0
inspectAddress
    :: (Alternative m, MonadThrow m)
    => Maybe XPub
    -> Address
    -> m Json.Value
inspectAddress mRootPub addr = either throwM (pure . toJSON) $
    eitherInspectAddress mRootPub addr

-- | Determines whether an 'Address' is a valid address for the Bcc Sophie
-- era. Sophie format addresses, as well as old-style Cole and Icarus
-- addresses can be parsed by this function.
--
-- Returns either details about the 'Address', or 'ErrInspectAddress' if it's
-- not a valid address.
--
-- @since 3.4.0
eitherInspectAddress
    :: Maybe XPub
    -> Address
    -> Either ErrInspectAddress InspectAddress
eitherInspectAddress mRootPub addr = unpackAddress addr >>= parseInfo
  where
    parseInfo :: AddressParts -> Either ErrInspectAddress InspectAddress
    parseInfo parts = case addrType parts of
        -- 1000: cole address
        0b10000000 ->
            (bimap ErrIcarus InspectAddressIcarus (Icarus.eitherInspectAddress addr))
            `orElse`
            (bimap ErrCole InspectAddressCole (Cole.eitherInspectAddress mRootPub addr))
        -- Anything else: sophie address
        _ -> bimap ErrSophie InspectAddressSophie (parseAddressInfoSophie parts)

-- | Returns either details about the 'Address', or
-- 'ErrInspectAddressOnlySophie' if it's not a valid Sophie address.
parseAddressInfoSophie :: AddressParts -> Either ErrInspectAddressOnlySophie AddressInfo
parseAddressInfoSophie AddressParts{..} = case addrType of
    -- 0000: base address: keyhash28,keyhash28
    0b00000000 | addrRestLength == credentialHashSize + credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Just ByValue
            , infoSpendingKeyHash = Just addrHash1
            , infoStakeKeyHash = Just addrHash2
            }
    -- 0001: base address: scripthash28,keyhash28
    0b00010000 | addrRestLength == credentialHashSize + credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Just ByValue
            , infoScriptHash = Just addrHash1
            , infoStakeKeyHash = Just addrHash2
            }
    -- 0010: base address: keyhash28,scripthash28
    0b00100000 | addrRestLength == credentialHashSize + credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Just ByValue
            , infoSpendingKeyHash = Just addrHash1
            , infoStakeScriptHash = Just addrHash2
            }
    -- 0011: base address: scripthash28,scripthash28
    0b00110000 | addrRestLength == 2 * credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Just ByValue
            , infoScriptHash = Just addrHash1
            , infoStakeScriptHash = Just addrHash2
            }
    -- 0100: pointer address: keyhash28, 3 variable length uint
    0b01000000 | addrRestLength > credentialHashSize -> do
        ptr <- getPtr addrHash2
        pure addressInfo
            { infoStakeReference = Just $ ByPointer ptr
            , infoSpendingKeyHash = Just addrHash1
            }
    -- 0101: pointer address: scripthash28, 3 variable length uint
    0b01010000 | addrRestLength > credentialHashSize -> do
        ptr <- getPtr addrHash2
        pure addressInfo
            { infoStakeReference = Just $ ByPointer ptr
            , infoScriptHash = Just addrHash1
            }
    -- 0110: enterprise address: keyhash28
    0b01100000 | addrRestLength == credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Nothing
            , infoSpendingKeyHash = Just addrHash1
            }
    -- 0111: enterprise address: scripthash28
    0b01110000 | addrRestLength == credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Nothing
            , infoScriptHash = Just addrHash1
            }
    -- 1110: reward account: keyhash28
    0b11100000 | addrRestLength == credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Just ByValue
            , infoStakeKeyHash = Just addrHash1
            }
    -- 1111: reward account: scripthash28
    0b11110000 | addrRestLength == credentialHashSize ->
        Right addressInfo
            { infoStakeReference = Just ByValue
            , infoScriptHash = Just addrHash1
            }
    unknown -> Left (UnknownType unknown)

  where
    addressInfo = AddressInfo
        { infoNetworkTag = NetworkTag $ fromIntegral addrNetwork
        , infoStakeReference = Nothing
        , infoSpendingKeyHash = Nothing
        , infoStakeKeyHash = Nothing
        , infoScriptHash = Nothing
        , infoStakeScriptHash = Nothing
        }

    getPtr :: ByteString -> Either ErrInspectAddressOnlySophie ChainPointer
    getPtr source = case runGetOrFail get (BL.fromStrict source) of
        Right ("", _, a) -> Right a
        Right _ -> err "Unconsumed bytes after pointer"
        Left (_, _, e) -> err e
      where
        get = ChainPointer
            <$> getVariableLengthNat
            <*> getVariableLengthNat
            <*> getVariableLengthNat
        err = Left . PtrRetrieveError

-- | The result of 'eitherInspectAddress'.
--
-- @since 3.4.0
data InspectAddress
    = InspectAddressSophie AddressInfo
    | InspectAddressIcarus Icarus.AddressInfo
    | InspectAddressCole Cole.AddressInfo
    deriving (Generic, Show, Eq)

instance ToJSON InspectAddress where
    toJSON addr = combine (styleProp <> missingProp) (toJSON addr')
      where
        addr' = case addr of
          InspectAddressSophie s -> toJSON s
          InspectAddressIcarus i -> toJSON i
          InspectAddressCole b -> toJSON b

        styleProp = "address_style" .= Json.String styleName
        styleName = case addr of
            InspectAddressSophie _ -> "Sophie"
            InspectAddressIcarus _ -> "Icarus"
            InspectAddressCole _ -> "Cole"
        missingProp = case addr of
            InspectAddressSophie _ -> mempty
            InspectAddressIcarus _ -> noStakeRef
            InspectAddressCole _ -> noStakeRef
        noStakeRef = "stake_reference" .= Json.String "none"

        combine extra = \case
            Json.Object props -> Json.Object (extra <> props)
            otherValue -> otherValue -- not expected to happen

-- | An inspected Sophie address.
--
-- @since 3.4.0
data AddressInfo = AddressInfo
    { infoStakeReference  :: !(Maybe ReferenceInfo)
    , infoSpendingKeyHash :: !(Maybe ByteString)
    , infoStakeKeyHash    :: !(Maybe ByteString)
    , infoScriptHash      :: !(Maybe ByteString)
    , infoStakeScriptHash :: !(Maybe ByteString)
    , infoNetworkTag      :: !NetworkTag
    } deriving (Generic, Show, Eq)

-- | Info from 'Address' about how delegation keys are located.
--
-- @since 3.3.0
data ReferenceInfo
    = ByValue
    | ByPointer ChainPointer
    deriving (Generic, Show, Eq)

instance ToJSON AddressInfo where
    toJSON AddressInfo{..} = Json.object $
        [ "network_tag" .= infoNetworkTag
        , "stake_reference" .= Json.String (maybe "none" refName infoStakeReference)
        ]
        ++ maybe [] (\ptr -> ["pointer" .= ptr]) (infoStakeReference >>= getPointer)
        ++ jsonHash "spending_key_hash" CIP5.addr_vkh infoSpendingKeyHash
        ++ jsonHash "stake_key_hash" CIP5.stake_vkh infoStakeKeyHash
        ++ jsonHash "spending_shared_hash" CIP5.addr_shared_vkh infoScriptHash
        ++ jsonHash "stake_shared_hash" CIP5.stake_shared_vkh infoScriptHash
        ++ jsonHash "stake_script_hash" CIP5.stake_vkh infoStakeScriptHash
      where
        getPointer ByValue = Nothing
        getPointer (ByPointer ptr) = Just ptr

        jsonHash _ _ Nothing = []
        jsonHash key hrp (Just bs) =
            [ key .= base16 bs , (key <> "_bech32") .= bech32With hrp bs ]

        base16 = T.unpack . T.decodeUtf8 . encode EBase16
        bech32With hrp = T.decodeUtf8 . encode (EBech32 hrp)

        refName ByValue = "by value"
        refName (ByPointer _) = "by pointer"

-- | Structure containing the result of 'unpackAddress', the constituent parts
-- of an address. Internal to this module.
data AddressParts = AddressParts
    { addrType :: Word8
    , addrNetwork :: Word8
    , addrHash1 :: ByteString
    , addrHash2 :: ByteString
    , addrRestLength :: Int
    } deriving (Show)

-- | Split fields out of a Sophie encoded address.
unpackAddress :: Address -> Either ErrInspectAddress AddressParts
unpackAddress (unAddress -> bytes)
    | BS.length bytes >= 1 + credentialHashSize = Right AddressParts{..}
    | otherwise = Left $ WrongInputSize $ BS.length bytes
  where
    (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes
    addrType = fstByte .&. 0b11110000
    addrNetwork = fstByte .&. 0b00001111
    (addrHash1, addrHash2) = BS.splitAt credentialHashSize rest
    addrRestLength = BS.length rest

-- | Sophie offers several ways to identify ownership of entities on chain.
--
-- This data-family has two instances, depending on whether the key is used for
-- payment or for delegation.
--
-- @since 3.0.0
data family Credential (purpose :: Depth)

data instance Credential 'PaymentK where
    PaymentFromKey :: Sophie 'PaymentK XPub -> Credential 'PaymentK
    PaymentFromScript :: ScriptHash -> Credential 'PaymentK
    deriving Show

data instance Credential 'DelegationK where
    DelegationFromKey :: Sophie 'DelegationK XPub -> Credential 'DelegationK
    DelegationFromScript :: ScriptHash -> Credential 'DelegationK
    DelegationFromPointer :: ChainPointer -> Credential 'DelegationK
    deriving Show

-- Re-export from 'Bcc.Address' to have it documented specialized in Haddock.
--
-- | Convert a payment credential (key or script) to a payment 'Address' valid
-- for the given network discrimination.
--
-- @since 2.0.0
paymentAddress
    :: NetworkDiscriminant Sophie
    -> Credential 'PaymentK
    -> Address
paymentAddress discrimination = \case
    PaymentFromKey keyPub ->
        constructPayload
            (EnterpriseAddress CredentialFromKey)
            discrimination
            (hashCredential . xpubPublicKey . getKey $ keyPub)
    PaymentFromScript (ScriptHash bytes) ->
        constructPayload
            (EnterpriseAddress CredentialFromScript)
            discrimination
            bytes

-- | Convert a payment credential (key or script) and a delegation credential (key or script)
-- to a delegation 'Address' valid for the given network discrimination.
-- Funds sent to this address will be delegated according to the delegation settings
-- attached to the delegation key.
--
-- @since 2.0.0
delegationAddress
    :: NetworkDiscriminant Sophie
    -> Credential 'PaymentK
    -> Credential 'DelegationK
    -> Address
delegationAddress discrimination paymentCredential stakeCredential =
    unsafeFromRight $ extendAddress
        (paymentAddress discrimination paymentCredential)
        stakeCredential

-- | Convert a payment credential (key or script) and pointer to delegation certificate in blockchain to a
-- pointer 'Address' valid for the given network discrimination.
--
-- @since 3.0.0
pointerAddress
    :: NetworkDiscriminant Sophie
    -> Credential 'PaymentK
    -> ChainPointer
    -> Address
pointerAddress discrimination credential pointer =
    unsafeFromRight $ extendAddress
        (paymentAddress discrimination credential)
        (DelegationFromPointer pointer)

-- | Convert a delegation credential (key or script) to a stake Address (aka reward account address)
-- for the given network discrimination.
--
-- @since 3.0.0
stakeAddress
    :: NetworkDiscriminant Sophie
    -> Credential 'DelegationK
    -> Either ErrInvalidStakeAddress Address
stakeAddress discrimination = \case
    DelegationFromKey keyPub ->
        Right $ constructPayload
            (RewardAccount CredentialFromKey)
            discrimination
            (hashCredential . xpubPublicKey . getKey $ keyPub)

    DelegationFromScript (ScriptHash bytes) ->
        Right $ constructPayload
            (RewardAccount CredentialFromScript)
            discrimination
            bytes

    DelegationFromPointer{} ->
        Left ErrStakeAddressFromPointer

-- | Stake addresses can only be constructed from key or script hash. Trying to
-- create one from a pointer will result in the following error.
--
-- @since 3.0.0
data ErrInvalidStakeAddress
    = ErrStakeAddressFromPointer
    deriving (Generic, Show, Eq)

-- | Extend an existing payment 'Address' to make it a delegation address.
--
-- @since 2.0.0
extendAddress
    :: Address
    -> Credential 'DelegationK
    -> Either ErrExtendAddress Address
extendAddress addr infoStakeReference = do
    when (isNothing (inspectAddress Nothing addr)) $
        Left $ ErrInvalidAddressStyle "Given address isn't a Sophie address"

    let bytes = unAddress addr
    let (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes

    let paymentFirstByte = fstByte .&. 0b11110000
    let extendableTypes = addressType <$>
            [ EnterpriseAddress CredentialFromKey
            , EnterpriseAddress CredentialFromScript
            ]
    unless (paymentFirstByte `elem` extendableTypes) $ do
        Left $ ErrInvalidAddressType "Only payment addresses can be extended"

    case infoStakeReference of
        -- base address: keyhash28,keyhash28    : 00000000 -> 0
        -- base address: scripthash32,keyhash28 : 00010000 -> 16
        DelegationFromKey delegationKey -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                -- 0b01100000 .&. 0b00011111 = 0
                -- 0b01110000 .&. 0b00011111 = 16
                putWord8 $ fstByte .&. 0b00011111
                putByteString rest
                putByteString . hashCredential . xpubPublicKey . getKey $ delegationKey

        -- base address: keyhash28,scripthash32    : 00100000 -> 32
        -- base address: scripthash32,scripthash32 : 00110000 -> 48
        DelegationFromScript (ScriptHash scriptBytes) -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                -- 0b01100000 .&. 0b00111111 = 32
                -- 0b01110000 .&. 0b00111111 = 48
                putWord8 $ fstByte .&. 0b00111111
                putByteString rest
                putByteString scriptBytes

        -- pointer address: keyhash28, 3 variable length uint    : 01000000 -> 64
        -- pointer address: scripthash32, 3 variable length uint : 01010000 -> 80
        DelegationFromPointer pointer -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                -- 0b01100000 .&. 0b01011111 = 64
                -- 0b01110000 .&. 0b01011111 = 80
                putWord8 $ fstByte .&. 0b01011111
                putByteString rest
                putPointer pointer
  where
    putPointer (ChainPointer a b c) = do
        putVariableLengthNat a
        putVariableLengthNat b
        putVariableLengthNat c

-- | Captures error occuring when trying to extend an invalid address.
--
-- @since 2.0.0
data ErrExtendAddress
    = ErrInvalidAddressStyle String
    | ErrInvalidAddressType String
    deriving (Show)

--
-- Network Discriminant
--

instance HasNetworkDiscriminant Sophie where
    type NetworkDiscriminant Sophie = NetworkTag
    addressDiscrimination _ = RequiresNetworkTag
    networkTag = id

-- | Error reported from trying to create a network discriminant from number
--
-- @since 2.0.0
newtype MkNetworkDiscriminantError
    = ErrWrongNetworkTag Integer
      -- ^ Wrong network tag.
    deriving (Eq, Show)

instance Buildable MkNetworkDiscriminantError where
  build (ErrWrongNetworkTag i) = "Invalid network tag "+|i|+". Must be between [0, 15]"

-- | Construct 'NetworkDiscriminant' for Bcc 'Sophie' from a number.
-- If the number is invalid, ie., not between 0 and 15, then
-- 'MkNetworkDiscriminantError' is thrown.
--
-- @since 2.0.0
mkNetworkDiscriminant
    :: Integer
    -> Either MkNetworkDiscriminantError (NetworkDiscriminant Sophie)
mkNetworkDiscriminant nTag
    | nTag < 16 =  Right $ NetworkTag $ fromIntegral nTag
    | otherwise = Left $ ErrWrongNetworkTag nTag

-- | Retrieve the network discriminant of a given 'Address'.
-- If the 'Address' is malformed or, not a sophie address, returns Nothing.
--
-- @since 2.0.0
inspectNetworkDiscriminant
    :: Address
    -> Maybe (NetworkDiscriminant Sophie)
inspectNetworkDiscriminant addr = case eitherInspectAddress Nothing addr of
    Right (InspectAddressSophie info) -> Just (infoNetworkTag info)
    _ -> Nothing

-- | 'NetworkDicriminant' for Bcc MainNet & Sophie
--
-- @since 2.0.0
sophieMainnet :: NetworkDiscriminant Sophie
sophieMainnet = NetworkTag 1

-- | 'NetworkDicriminant' for Bcc Testnet & Sophie
--
-- @since 2.0.0
sophieTestnet :: NetworkDiscriminant Sophie
sophieTestnet = NetworkTag 0

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Sophie' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPrv :: XPrv -> Sophie depth XPrv
liftXPrv = Sophie

-- | Unsafe backdoor for constructing an 'Sophie' key from a raw 'XPub'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPub :: XPub -> Sophie depth XPub
liftXPub = Sophie

-- Use with care when it is _safe_.
unsafeFromRight :: Either a c -> c
unsafeFromRight =
    either (error "impossible: internally generated invalid address") id

--
-- Internal
--

-- Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Bcc:
--
-- https://github.com/The-Blockchain-Company/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073c

-- One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Bcc this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Bcc
-- Entropic.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- The minimum seed length for 'genMasterKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- A sum-type for constructing addresses payment part.
data CredentialType = CredentialFromKey | CredentialFromScript
    deriving (Show, Eq)

-- Different types of Sophie addresses.
data AddressType
    = BaseAddress CredentialType CredentialType
    | PointerAddress CredentialType
    | EnterpriseAddress CredentialType
    | RewardAccount CredentialType
    | ColeAddress
    deriving (Show, Eq)

addressType :: AddressType -> Word8
addressType = \case
    ColeAddress                                                -> 0b10000000
    BaseAddress       CredentialFromKey    CredentialFromKey    -> 0b00000000
    BaseAddress       CredentialFromScript CredentialFromKey    -> 0b00010000
    BaseAddress       CredentialFromKey    CredentialFromScript -> 0b00100000
    BaseAddress       CredentialFromScript CredentialFromScript -> 0b00110000
    PointerAddress    CredentialFromKey                         -> 0b01000000
    PointerAddress    CredentialFromScript                      -> 0b01010000
    EnterpriseAddress CredentialFromKey                         -> 0b01100000
    EnterpriseAddress CredentialFromScript                      -> 0b01110000
    RewardAccount                          CredentialFromKey    -> 0b11100000
    RewardAccount                          CredentialFromScript -> 0b11110000

-- Helper to constructs appropriate address headers. Rest of the payload is left
-- to the caller as a raw 'ByteString'.
constructPayload
    :: AddressType
    -> NetworkDiscriminant Sophie
    -> ByteString
    -> Address
constructPayload addrType discrimination bytes = unsafeMkAddress $
    invariantSize expectedLength $ BL.toStrict $ runPut $ do
        putWord8 firstByte
        putByteString bytes
  where
    firstByte =
        let netTagLimit = 16
        in addressType addrType + invariantNetworkTag netTagLimit (networkTag @Sophie discrimination)
    expectedLength =
        let headerSizeBytes = 1
        in headerSizeBytes + credentialHashSize

--Sophie specific derivation and generation
genMasterKeyFromMnemonicSophie
    :: BA.ByteArrayAccess sndFactor
    => SomeMnemonic
    -> sndFactor
    -> XPrv
genMasterKeyFromMnemonicSophie fstFactor =
    generateNew seedValidated
    where
        seed  = someMnemonicToBytes fstFactor
        seedValidated = assert
            (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
            seed

deriveAccountPrivateKeySophie
    :: XPrv
    -> Index derivationType depth
    -> Word32
    -> XPrv
deriveAccountPrivateKeySophie rootXPrv accIx purpose =
    let
        Just purposeIx =
            indexFromWord32 @(Index 'Hardened _) purpose
        Just coinTypeIx =
            indexFromWord32 @(Index 'Hardened _) coinTypeIndex
        purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
            deriveXPrv DerivationScheme2 rootXPrv purposeIx
        coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
            deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
        acctXPrv = -- lvl3 derivation; hardened derivation of account' index
            deriveXPrv DerivationScheme2 coinTypeXPrv accIx
    in
        acctXPrv

deriveAddressPrivateKeySophie
    :: XPrv
    -> Role
    -> Index derivationType depth
    -> XPrv
deriveAddressPrivateKeySophie accXPrv role addrIx =
    let
        changeXPrv = -- lvl4 derivation; soft derivation of change chain
            deriveXPrv DerivationScheme2 accXPrv (roleToIndex role)
        addrXPrv = -- lvl5 derivation; soft derivation of address index
            deriveXPrv DerivationScheme2 changeXPrv addrIx
    in
        addrXPrv

deriveAddressPublicKeySophie
    :: XPub
    -> Role
    -> Index derivationType depth
    -> XPub
deriveAddressPublicKeySophie accXPub role addrIx =
    fromMaybe errWrongIndex $ do
        changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
            deriveXPub DerivationScheme2 accXPub (roleToIndex role)
        -- lvl5 derivation in bip44 is derivation of address chain
        deriveXPub DerivationScheme2 changeXPub addrIx
  where
      errWrongIndex = error $
          "deriveAddressPublicKey failed: was given an hardened (or too big) \
          \index for soft path derivation ( " ++ show addrIx ++ "). This is \
          \either a programmer error, or, we may have reached the maximum \
          \number of addresses for a given wallet."
