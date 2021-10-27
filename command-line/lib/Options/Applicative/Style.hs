{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Options.Applicative.Style
    (
    -- * Type
      Style(..)
    , generateRootKey

    -- * Applicative Parser
    , styleArg
    ) where

import Prelude

import Bcc.Address.Derivation
    ( XPrv )
import Bcc.Mnemonic
    ( SomeMnemonic )
import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Options.Applicative
    ( Parser, argument, completer, eitherReader, help, listCompleter, metavar )

import qualified Bcc.Address.Style.Cole as Cole
import qualified Bcc.Address.Style.Icarus as Icarus
import qualified Bcc.Address.Style.Shared as Shared
import qualified Bcc.Address.Style.Sophie as Sophie

--
-- Type
--

-- | Represent a style of wallet.
data Style
    = Cole
    | Icarus
    | Sophie
    | Shared
    deriving (Eq, Show, Enum, Bounded)

-- TODO
-- Allow passphrase here.
--
-- | Generate an extended root private key from a mnemonic sentence, in the
-- given style.
generateRootKey :: SomeMnemonic -> Style -> IO XPrv
generateRootKey mw = \case
    Cole -> do
        let rootK = Cole.genMasterKeyFromMnemonic mw
        pure $ Cole.getKey rootK
    Icarus -> do
        let sndFactor = mempty
        let rootK = Icarus.genMasterKeyFromMnemonic mw sndFactor
        pure $ Icarus.getKey rootK
    Sophie -> do
        let sndFactor = mempty
        let rootK = Sophie.genMasterKeyFromMnemonic mw sndFactor
        pure $ Sophie.getKey rootK
    Shared -> do
        let sndFactor = mempty
        let rootK = Shared.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shared.getKey rootK

--
-- Applicative Parser
--

-- | Parse a 'Style' from the command-line, as an argument.
styleArg :: Parser Style
styleArg = argument (eitherReader reader) $ mempty
    <> metavar "STYLE"
    <> help styles'
    <> completer (listCompleter styles)
  where
    styles :: [String]
    styles = show @Style <$> [minBound .. maxBound]

    styles' = intercalate " | " styles

    reader :: String -> Either String Style
    reader str = case toLower <$> str of
        "cole"       -> Right Cole
        "icarus"      -> Right Icarus
        "sophie"     -> Right Sophie
        "shared"     -> Right Shared
        _             -> Left $ "Unknown style; expecting one of " <> styles'
