{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Bootstrap
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Bcc.Address
    ( AddressDiscrimination (..), NetworkDiscriminant, base58 )
import Bcc.Address.Derivation
    ( XPub, coerceWholeDomainIndex )
import Bcc.Address.Style.Cole
    ( Cole )
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , footerDoc
    , header
    , helper
    , info
    , optional
    , progDesc
    )
import Options.Applicative.Derivation
    ( DerivationPath, castDerivationPath, derivationPathArg, xpubOpt )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Style
    ( Style (..) )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPub, progName )

import qualified Bcc.Address.Style.Cole as Cole
import qualified Bcc.Address.Style.Icarus as Icarus
import qualified Bcc.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

data Cmd = Cmd
    { rootXPub :: Maybe XPub
    , networkTag :: NetworkTag
    , derivationPath :: Maybe DerivationPath
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "bootstrap" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a bootstrap address"
        <> header "Those addresses, now deprecated, were used during the Cole era."
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 12 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Cole > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 14H/42H | tee addr.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address bootstrap --root $(cat root.prv | "<>progName<>" key public --with-chain-code) \\"
            , indent 8 $ bold $ string "--network-tag testnet 14H/42H"
            , indent 2 $ string "DdzFFzCqrht2KG1vWt5WGhVC9Ezyu32RgB5M2DocdZ6BQU6zj69LSqksDmdM..."
            ])
  where
    parser = Cmd
        <$> optional (xpubOpt [CIP5.root_xvk] "root" "A root public key. Needed for Cole addresses only.")
        <*> networkTagOpt Cole
        <*> optional derivationPathArg

run :: Cmd -> IO ()
run Cmd{networkTag,rootXPub,derivationPath} = do
    (_hrp, xpub) <- hGetXPub stdin [CIP5.addr_xvk]
    addr <- case derivationPath of
        Nothing ->
            pure $ Icarus.paymentAddress discriminant (Icarus.liftXPub xpub)
        Just untypedPath -> do
            root <- maybe
                (fail "A root public key must be provided when --path is provided") pure rootXPub
            case castDerivationPath untypedPath of
                [acctIx, addrIx] -> do
                    let path = (acctIx, coerceWholeDomainIndex addrIx)
                        xkey = Cole.liftXPub root path xpub
                    pure $ Cole.paymentAddress discriminant xkey
                _ -> do
                    fail "Cole derivation path must describe 2 levels (e.g. 0H/0H)"
    B8.hPutStr stdout $ T.encodeUtf8 $ base58 addr
  where
    discriminant :: NetworkDiscriminant Cole -- Or Icarus, same.
    discriminant
        | networkTag == snd Cole.coleMainnet =
            Cole.coleMainnet
        | otherwise =
            (RequiresNetworkTag, networkTag)
