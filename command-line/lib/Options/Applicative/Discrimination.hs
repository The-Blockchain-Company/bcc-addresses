{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Options.Applicative.Discrimination
    (
    -- * Type (re-export from Bcc.Address)
      NetworkTag(..)
    , fromNetworkTag

    -- * Applicative Parser
    , networkTagOpt
    ) where

import Prelude

import Bcc.Address
    ( NetworkDiscriminant (..), NetworkTag (..) )
import Bcc.Address.Style.Sophie
    ( Sophie )
import Data.List
    ( intercalate )
import Options.Applicative
    ( Parser
    , completer
    , eitherReader
    , helpDoc
    , listCompleter
    , long
    , metavar
    , option
    , (<|>)
    )
import Options.Applicative.Help.Pretty
    ( string, vsep )
import Options.Applicative.Style
    ( Style (..) )
import Text.Read
    ( readMaybe )

import qualified Bcc.Address.Style.Cole as Cole
import qualified Bcc.Address.Style.Sophie as Sophie

-- | Construct a Sophie 'NetworkDiscriminant' from a network tag. Fails loudly
-- if not possible.
fromNetworkTag :: MonadFail m => NetworkTag -> m (NetworkDiscriminant Sophie)
fromNetworkTag tag =
    case (Sophie.mkNetworkDiscriminant . fromIntegral . unNetworkTag) tag of
        Left Sophie.ErrWrongNetworkTag{} -> do
            fail "Invalid network tag. Must be between [0, 15]"
        Right discriminant ->
            pure discriminant

--
-- Applicative Parser
--

-- | Parse a 'NetworkTag' from the command-line, as an option
networkTagOpt :: Style -> Parser NetworkTag
networkTagOpt style = option (eitherReader reader) $ mempty
    <> metavar "NETWORK-TAG"
    <> long "network-tag"
    <> helpDoc  (Just (vsep (string <$> doc style)))
    <> completer (listCompleter $ show <$> tagsFor style)
  where
    doc style' =
        [ "A tag which identifies a Bcc network."
        , ""
        , header
        ]
        ++ (fmtAllowedKeyword <$> ("" : allowedKeywords style'))
        ++
        [ ""
        , "...or alternatively, an explicit network tag as an integer."
        ]
      where
        header = case style' of
            Cole ->
                "┌ Cole / Icarus ──────────"
            Icarus ->
                "┌ Cole / Icarus ──────────"
            Sophie ->
                "┌ Sophie ─────────────────"
            Shared ->
                "┌ Shared ──────────────────"
        fmtAllowedKeyword network =
            "│ " <> network

    tagsFor = \case
        Cole ->
            [ unNetworkTag (snd Cole.coleMainnet)
            , unNetworkTag (snd Cole.coleStaging)
            , unNetworkTag (snd Cole.coleTestnet)
            ]
        Icarus ->
            tagsFor Cole
        Sophie ->
            [ unNetworkTag Sophie.sophieMainnet
            , unNetworkTag Sophie.sophieTestnet
            ]
        Shared ->
            [ unNetworkTag Sophie.sophieMainnet
            , unNetworkTag Sophie.sophieTestnet
            ]

    reader str = maybe (Left err) Right
        ((NetworkTag <$> readMaybe str) <|> (readKeywordMaybe str style))
      where
        err =
            "Invalid network tag. Must be an integer value or one of the \
            \allowed keywords: " <> intercalate ", " (allowedKeywords style)

    readKeywordMaybe str = \case
        Cole | str == "mainnet" -> pure (snd Cole.coleMainnet)
        Cole | str == "staging" -> pure (snd Cole.coleStaging)
        Cole | str == "testnet" -> pure (snd Cole.coleTestnet)
        Icarus -> readKeywordMaybe str Cole
        Sophie | str == "mainnet" -> pure Sophie.sophieMainnet
        Sophie | str == "testnet" -> pure Sophie.sophieTestnet
        Shared | str == "mainnet" -> pure Sophie.sophieMainnet
        Shared | str == "testnet" -> pure Sophie.sophieTestnet
        _ -> Nothing

    allowedKeywords = \case
        Cole -> ["mainnet", "staging", "testnet"]
        Icarus -> allowedKeywords Cole
        Sophie -> ["mainnet", "testnet"]
        Shared -> ["mainnet", "testnet"]
