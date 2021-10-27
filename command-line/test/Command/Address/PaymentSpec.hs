{-# LANGUAGE FlexibleContexts #-}

module Command.Address.PaymentSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "address", "payment" ] $ do
    specSophie defaultPhrase "1852H/1815H/0H/0/0" "0"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specSophie defaultPhrase "1852H/1815H/0H/0/0" "3"
        "addr1vdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0m9a08"

    specSophie defaultPhrase "1852H/1815H/0H/0/0" "testnet"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specSophie defaultPhrase "1852H/1815H/0H/0/0" "mainnet"
        "addr1v9u5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0kvk0f"

    specMalformedNetwork "💩"

    specInvalidNetwork "42"
    specInvalidNetwork "staging"

specSophie :: [String] -> String -> String -> String -> SpecWith ()
specSophie phrase path networkTag want = it ("golden sophie (payment) " <> path) $ do
    out <- cli [ "key", "from-recovery-phrase", "sophie" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` want

specMalformedNetwork :: String -> SpecWith ()
specMalformedNetwork networkTag = it ("malformed network " <> networkTag) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", "sophie" ] (unwords defaultPhrase)
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag. Must be an integer value or one of the allowed keywords:"
    err `shouldContain` "Usage"

specInvalidNetwork :: String -> SpecWith ()
specInvalidNetwork networkTag = it ("invalid network " <> networkTag) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", "sophie" ] (unwords defaultPhrase)
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag."

defaultPhrase :: [String]
defaultPhrase =
    [ "art", "forum", "devote", "street", "sure"
    , "rather", "head", "chuckle", "guard", "poverty"
    , "release", "quote", "oak", "craft", "enemy"
    ]
