{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe (fromJust)

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import           Network.Multiaddr

instance QC.Arbitrary IPv4 where
  arbitrary = fromBytes <$> QC.arbitrary

instance QC.Arbitrary IPv6 where
  arbitrary = do
    (a, b, c, d) <- QC.arbitrary
    (e, f, g, h) <- QC.arbitrary
    pure (fromPieces (a, b, c, d, e, f, g, h))

ipv4Tests :: TestTree
ipv4Tests = testGroup "IPv4"
  [ QC.testProperty "round trips" $
    \ipv4 -> readIPv4 (toText (ipv4 :: IPv4)) == Just ipv4

  , testCase "parses and formats sample IPs" $
    mapM_ (\t -> toText (fromJust (readIPv4 t)) @?= t)
    [ "0.0.0.0"
    , "0.0.0.1"
    , "1.1.1.1"
    , "127.0.0.1"
    , "192.168.1.1"
    , "255.255.255.0"
    , "255.255.255.255"
    ]

  , testCase "fails to parse bad IPs" $ mapM_ (\t -> readIPv4 t @?= Nothing)
    [ ""
    , "foo"
    , "..."
    , "127 0 0 1"
    , "1 27.0.0.1"
    , "256.0.0.0"
    , "-1.0.0.0"
    , "1.2.3.4.5"
    , "1.2.3.ff"
    ]

  , testCase "Bounded instance" $ do
      (minBound :: IPv4) @?= IPv4 0
      (maxBound :: IPv4) @?= IPv4 0xFFFFFFFF
  ]

ipv6Tests :: TestTree
ipv6Tests = testGroup "IPv6"
  [ testCase "parses sample IPs" $ do
      readIPv6 "::" @?= Just (IPv6 0 0 0 0)
      readIPv6 "::1" @?= Just (IPv6 0 0 0 1)
      readIPv6 "::2" @?= Just (IPv6 0 0 0 2)
      readIPv6 "::ffff" @?= Just (IPv6 0 0 0 0xFFFF)
      readIPv6 "::ff:ffff" @?= Just (IPv6 0 0 0 0xFFFFFF)
      readIPv6 "::ffff:ffff" @?= Just (IPv6 0 0 0 0xFFFFFFFF)
      readIPv6 "2001:0DB8:AC10:FE01:0000:0000:0000:0000" @?=
        Just (IPv6 0x20010DB8 0xAC10FE01 0 0)
      readIPv6 "2001:0DB8:AC10:FE01:0:0:0:0" @?=
        Just (IPv6 0x20010DB8 0xAC10FE01 0 0)
      readIPv6 "2001:db8:ac10:fe01::" @?=
        Just (IPv6 0x20010DB8 0xAC10FE01 0 0)

  , testCase "round trips RFC 5952 IPs" $
    mapM_ (\t -> toText (fromJust (readIPv6 t)) @?= t)
    [ "::"
    , "::1"
    , "::1:1"
    , "1::"
    , "1::1"
    , "1::1:1"
    , "2001:db8:85a3:8d3:1319:8a2e:370:7348"
    ]

  , QC.testProperty "round trips arbitrary IPs" $
    \ip -> readIPv6 (toText (ip :: IPv6)) == Just ip

  , testCase "Bounded instance" $ do
      (minBound :: IPv6) @?= IPv6 0 0 0 0
      (maxBound :: IPv6) @?= IPv6 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF
  ]

tests :: TestTree
tests = testGroup "Network.Multiaddr" [ipv4Tests, ipv6Tests]

main :: IO ()
main = defaultMain tests
