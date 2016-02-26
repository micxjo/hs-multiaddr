{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module          : Network.Multiaddr
Description     : A network address format
Copyright       : (c) 2016 Micxjo Funkcio
License         : BSD3
Maintainer      : micxjo@fastmail.com
Stability       : Experimental

Multiaddr is a self-describing network address format supporting a
variety of protocols, with both string and binary representations.
-}
module Network.Multiaddr
       ( -- * Multiaddr type
         Multiaddr
         -- * Encoding / decoding
       , readMultiaddr
       , encode
       , decode
       , TextAddr(..)
         -- * Encapsulation
       , encapsulate
         -- * Query
       , parts
       , protocolNames
       , hasIPv4
       , hasIPv6
       , hasUDP
       , hasTCP
       , hasUDT
       , hasUTP
       , hasDCCP
       , hasSCTP
       , hasIPFS

         -- * MultiaddrPart type
       , MultiaddrPart(..)

         -- * IPv4 type
       , IPv4Addr(..)
       , readIPv4Addr
       , fromBytes

         -- * IPv6 type
       , IPv6Addr(..)
       , readIPv6Addr
       , fromPieces
       ) where

import           Control.Applicative (many)
import           Control.Monad (guard)
import           Data.Bits
import           Data.List (intersperse, scanl', elemIndex)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Word (Word32, Word16, Word8, Word64)

import           Control.Error (hush)
import           Data.Attoparsec.Text hiding (take)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Base58 (encodeBase58, decodeBase58,
                                         bitcoinAlphabet)
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize(..), Get, getBytes)
import qualified Data.Serialize as Cereal
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, toLazyText, singleton,
                                         fromText)
import qualified Data.Text.Lazy.Builder.Int as Builder

import           Data.Serialize.Varint

-- | An IPv4 address.
data IPv4Addr = IPv4Addr {-# UNPACK #-} !Word32
          deriving (Eq, Ord, Bounded, Generic, Typeable)

instance Hashable IPv4Addr

instance Serialize IPv4Addr where
  get = IPv4Addr <$> get
  put (IPv4Addr w) = put w

-- | An IPv6 address.
data IPv6Addr = IPv6Addr
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
          deriving (Eq, Ord, Bounded, Generic, Typeable)

instance Hashable IPv6Addr

instance Serialize IPv6Addr where
  get = IPv6Addr <$> get <*> get <*> get <*> get
  put (IPv6Addr a b c d) = put a >> put b >> put c >> put d

asBytes :: IPv4Addr -> [Word8]
asBytes (IPv4Addr ip) =
  map (\n -> fromIntegral (ip `shiftR` n) .&. 0xFF) [24, 16, 8, 0]

asPieces :: IPv6Addr -> [Word16]
asPieces (IPv6Addr a b c d) =
  map fromIntegral [ a `shiftR` 16
                   , a .&. 0xFFFF
                   , b `shiftR` 16
                   , b .&. 0xFFFF
                   , c `shiftR` 16
                   , c .&. 0xFFFF
                   , d `shiftR` 16
                   , d .&. 0xFFFF
                   ]

fromBytes :: (Word8, Word8, Word8, Word8) -> IPv4Addr
fromBytes (a, b, c, d) = IPv4Addr ( shiftL (fromIntegral a) 24
                                    `xor` shiftL (fromIntegral b) 16
                                    `xor` shiftL (fromIntegral c) 8
                                    `xor` fromIntegral d)

fromPieces :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
               -> IPv6Addr
fromPieces (a, b, c, d, e, f, g, h) =
    IPv6Addr (word a b) (word c d) (word e f) (word g h)
  where word x y = (fromIntegral x `shiftL` 16) `xor` fromIntegral y

word8P :: Parser Word8
word8P = do
  w <- decimal :: Parser Integer
  guard (w >= 0 && w <= 255)
  pure (fromIntegral w)

ipv4AddrP :: Parser IPv4Addr
ipv4AddrP = do
  a <- word8P <* char '.'
  b <- word8P <* char '.'
  c <- word8P <* char '.'
  d <- word8P
  pure (fromBytes (a, b, c, d))

-- | Try to read an IPv4 address (e.g. @"192.168.1.2"@)
readIPv4Addr :: Text -> Maybe IPv4Addr
readIPv4Addr = hush . parseOnly (ipv4AddrP <* endOfInput)

word16P :: Parser Word16
word16P = do
  w <- hexadecimal :: Parser Integer
  guard (w >= 0 && w <= 65535)
  pure (fromIntegral w)

ipv6AddrP :: Parser IPv6Addr
ipv6AddrP = do
    heads <- sepBy' word16P (char ':')
    _ <- option ':' (char ':' *> char ':')
    tails <- sepBy' word16P (char ':')
    let len = length heads + length tails
    guard (len <= 8)
    let pieces = heads ++ replicate (8 - len) 0 ++ tails
    pure (fromPieces' pieces)
  where fromPieces' [a, b, c, d, e, f, g, h] =
          fromPieces (a, b, c, d, e, f, g, h)
        fromPieces' _ = error "unreachable"

-- | Try to read an IPv6 address
-- (e.g. @"2001:db8:85a3:8d3:1319:8a2e:370:7348"@)
readIPv6Addr :: Text -> Maybe IPv6Addr
readIPv6Addr = hush . parseOnly (ipv6AddrP <* endOfInput)

ipv4AddrB :: IPv4Addr -> Builder
ipv4AddrB =
  mconcat . intersperse (singleton '.') . map Builder.decimal . asBytes

{- All we need for ipv6B to pass LiquidHaskell -}
{-@ assert scanl' :: (b -> a -> b) -> b -> xs:[a]
                     -> {v : [b] | len(v) = 1 + len(xs) } @-}

ipv6AddrB :: IPv6Addr -> Builder
ipv6AddrB ip =
  if maxRun <= 1
     then trans pieces
     else trans start <> fromText "::" <> trans end
  where pieces = asPieces ip
        runs = tail (scanl' (\x y -> if y == 0 then x + 1 else 0) 0 pieces)
        maxRun = maximum runs
        maxInd = (fromJust (elemIndex maxRun runs)) - maxRun + 1
        start = take maxInd pieces
        end = drop (maxInd + maxRun) pieces
        trans = mconcat . intersperse (singleton ':') . map Builder.hexadecimal

-- | Render an address to its standard text representation.
class TextAddr a where
  toText :: a -> Text

instance TextAddr IPv4Addr where
  toText = toStrict . toLazyText . ipv4AddrB

-- | RFC 5952 Compliant
instance TextAddr IPv6Addr where
  toText = toStrict . toLazyText . ipv6AddrB

instance Show IPv4Addr where
  show = T.unpack . toText

instance Show IPv6Addr where
  show = T.unpack . toText

-- | An individual component of a multiaddr.
data MultiaddrPart = IPv4 !IPv4Addr
              | IPv6 !IPv6Addr
              | UDP !Word16
              | TCP !Word16
              | DCCP !Word16
              | SCTP !Word16
              | IPFS !ByteString
              | UDT
              | UTP
              | HTTP
              | HTTPS
              deriving (Eq, Generic, Typeable)

instance Hashable MultiaddrPart

instance Serialize MultiaddrPart where
  put (IPv4 ip) = put (4 :: Varint Word32) >> put ip
  put (IPv6 ip) = put (41 :: Varint Word32) >> put ip
  put (UDP port) = put (17 :: Varint Word32) >> put port
  put (TCP port) = put (6 :: Varint Word32) >> put port
  put (DCCP port) = put (33 :: Varint Word32) >> put port
  put (SCTP port) = put (132 :: Varint Word32) >> put port
  put (IPFS addr) = do
    put (421 :: Varint Word32)
    case decodeBase58 bitcoinAlphabet addr of
      Nothing -> fail "invalid base58"
      Just bytes -> do
        put (fromIntegral (BS.length bytes) :: Varint Word64)
        mapM_ put (BS.unpack bytes)
  put UDT = put (301 :: Varint Word32)
  put UTP = put (302 :: Varint Word32)
  put HTTP = put (480 :: Varint Word32)
  put HTTPS = put (443 :: Varint Word32)

  get = do
    code <- get :: Get (Varint Word32)
    case code of
      4 -> IPv4 <$> get
      6 -> TCP <$> get
      17 -> UDP <$> get
      33 -> DCCP <$> get
      41 -> IPv6 <$> get
      132 -> SCTP <$> get
      421 -> do
        len <- get :: Get (Varint Word64)
        bytes <- getBytes (fromIntegral len)
        let base58 = encodeBase58 bitcoinAlphabet bytes
        pure (IPFS base58)
      301 -> pure UDT
      302 -> pure UTP
      480 -> pure HTTP
      443 -> pure HTTPS
      _ -> fail "invalid multiaddr code"

-- | A network address.
newtype Multiaddr = Multiaddr { _parts :: [MultiaddrPart] }
                  deriving (Eq, Monoid, Generic, Typeable)

instance Hashable Multiaddr

instance Serialize Multiaddr where
  get = Multiaddr <$> many get

  put (Multiaddr ps) = mapM_ put ps

-- | Get the individual parts of the multiaddr, in order
-- (e.g. @["\/ip4\/8.8.8.8", "\/tcp\/80"]@)
parts :: Multiaddr -> [MultiaddrPart]
parts = _parts

ipv4PartP :: Parser MultiaddrPart
ipv4PartP = IPv4 <$> (string "/ip4/" *> ipv4AddrP)

ipv6PartP :: Parser MultiaddrPart
ipv6PartP = IPv6 <$> (string "/ip6/" *> ipv6AddrP)

udpPartP :: Parser MultiaddrPart
udpPartP = do
  _ <- string "/udp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (UDP (fromIntegral port))

dccpPartP :: Parser MultiaddrPart
dccpPartP = do
  _ <- string "/dccp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (DCCP (fromIntegral port))

sctpPartP :: Parser MultiaddrPart
sctpPartP = do
  _ <- string "/sctp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (SCTP (fromIntegral port))

udtPartP :: Parser MultiaddrPart
udtPartP = string "/udt" >> pure UDT

utpPartP :: Parser MultiaddrPart
utpPartP = string "/utp" >> pure UTP

httpPartP :: Parser MultiaddrPart
httpPartP = string "/http" >> pure HTTP

httpsPartP :: Parser MultiaddrPart
httpsPartP = string "/https" >> pure HTTPS

tcpPartP :: Parser MultiaddrPart
tcpPartP = do
  _ <- string "/tcp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (TCP (fromIntegral port))

ipfsPartP :: Parser MultiaddrPart
ipfsPartP = do
  _ <- string "/ipfs/"
  addr <- takeTill (== '/')
  pure (IPFS (T.encodeUtf8 addr))

multiaddrP :: Parser Multiaddr
multiaddrP = do
  _parts <- many' (choice [ ipv4PartP
                          , ipv6PartP
                          , ipfsPartP
                          , udpPartP
                          , tcpPartP
                          , dccpPartP
                          , sctpPartP
                          , udtPartP
                          , utpPartP
                          , httpPartP
                          , httpsPartP
                          ])
  pure Multiaddr{..}

-- | Try to read a multiaddr in the standard text format
-- (e.g. @"\/ip4\/8.8.8.8\/tcp\/80"@)
readMultiaddr :: Text -> Maybe Multiaddr
readMultiaddr = hush . parseOnly (multiaddrP <* endOfInput)

addrPartB :: MultiaddrPart -> Builder
addrPartB (IPv4 ip) = fromText "/ip4/" <> ipv4AddrB ip
addrPartB (IPv6 ip) = fromText "/ip6/" <> ipv6AddrB ip
addrPartB (UDP port) = fromText "/udp/" <> Builder.decimal port
addrPartB (TCP port) = fromText "/tcp/" <> Builder.decimal port
addrPartB (DCCP port) = fromText "/dccp/" <> Builder.decimal port
addrPartB (SCTP port) = fromText "/sctp/" <> Builder.decimal port
addrPartB (IPFS addr) = fromText "/ipfs/" <> fromText (T.decodeUtf8 addr)
addrPartB UDT = fromText "/udt"
addrPartB UTP = fromText "/utp"
addrPartB HTTP = fromText "/http"
addrPartB HTTPS = fromText "/https"

instance TextAddr MultiaddrPart where
  toText = toStrict . toLazyText . addrPartB

instance Show MultiaddrPart where
  show = T.unpack . toText

multiaddrB :: Multiaddr -> Builder
multiaddrB (Multiaddr ps) = mconcat (map addrPartB ps)

instance TextAddr Multiaddr where
  toText = toStrict . toLazyText . multiaddrB

instance Show Multiaddr where
  show = T.unpack . toText

-- | Encode a multiaddr using the standard binary represenation.
encode :: Multiaddr -> ByteString
encode = Cereal.encode

-- | Try to decode a binary-encoded multiaddr.
decode :: ByteString -> Maybe Multiaddr
decode = hush . Cereal.decode

-- | Does the multiaddr contain an IPv4 part?
hasIPv4 :: Multiaddr -> Bool
hasIPv4 (Multiaddr ps) = any (\case IPv4 _ -> True; _ -> False) ps

-- | Does the multiaddr contain an IPv6 part?
hasIPv6 :: Multiaddr -> Bool
hasIPv6 (Multiaddr ps) = any (\case IPv6 _ -> True; _ -> False) ps

-- | Does the multiaddr contain a TCP part?
hasTCP :: Multiaddr -> Bool
hasTCP (Multiaddr ps) = any (\case TCP _ -> True; _ -> False) ps

-- | Does the multiaddr contain a UDP part?
hasUDP :: Multiaddr -> Bool
hasUDP (Multiaddr ps) = any (\case UDP _ -> True; _ -> False) ps

-- | Does the multiaddr contain a UDT part?
hasUDT :: Multiaddr -> Bool
hasUDT (Multiaddr ps) = any (== UDT) ps

-- | Does the multiaddr contain a UTP part?
hasUTP :: Multiaddr -> Bool
hasUTP (Multiaddr ps) = any (== UTP) ps

-- | Does the multiaddr contain a DCCP part?
hasDCCP :: Multiaddr -> Bool
hasDCCP (Multiaddr ps) = any (\case DCCP _ -> True; _ -> False) ps

-- | Does the multiaddr contain an SCTP part?
hasSCTP :: Multiaddr -> Bool
hasSCTP (Multiaddr ps) = any (\case SCTP _ -> True; _ -> False) ps

-- | Does the multiaddr contain an IPFS part?
hasIPFS :: Multiaddr -> Bool
hasIPFS (Multiaddr ps) = any (\case IPFS _ -> True; _ -> False) ps

-- | Append two multiaddrs (alias for @(<>)@)
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate = mappend

-- | Get an ordered list of the protocols specified by the multiaddr.
-- Protocols can appear more than once if they are repeated in the multiaddr.
protocolNames :: Multiaddr -> [Text]
protocolNames (Multiaddr ms) = map protoName ms
  where protoName (IPv4 _) = "ip4"
        protoName (IPv6 _) = "ip6"
        protoName (UDP _) = "udp"
        protoName (TCP _) = "tcp"
        protoName (DCCP _) = "dccp"
        protoName (SCTP _) = "sctp"
        protoName (IPFS _) = "ipfs"
        protoName UDT = "udt"
        protoName UTP = "utp"
        protoName HTTP = "http"
        protoName HTTPS = "https"
