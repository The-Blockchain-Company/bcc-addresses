{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Credential
    ( delegationCredentialArg
    ) where

import Prelude

import Bcc.Address.Derivation
    ( Depth (..) )
import Bcc.Address.Internal
    ( orElse )
import Bcc.Address.Style.Sophie
    ( Credential (..), liftXPub )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Options.Applicative.Derivation
    ( xpubReader )
import Options.Applicative.Script
    ( scriptHashReader )

import qualified Bcc.Codec.Bech32.Prefixes as CIP5

--
-- Applicative Parser
--

delegationCredentialArg  :: String -> Parser (Credential 'DelegationK)
delegationCredentialArg helpDoc = argument (eitherReader reader) $ mempty
    <> metavar "KEY || SCRIPT HASH"
    <> help helpDoc
  where
    reader :: String -> Either String (Credential 'DelegationK)
    reader str =
       (DelegationFromKey . liftXPub <$> xpubReader allowedPrefixes str)
       `orElse`
       (DelegationFromScript <$> scriptHashReader str)
       `orElse`
       Left "Couldn't parse delegation credentials. Neither a public key nor a script hash."

    -- TODO: Allow non-extended keys here.
    allowedPrefixes =
        [ CIP5.stake_xvk
        ]
