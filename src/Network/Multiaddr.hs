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
       , hasDCCP
       , hasSCTP
       , hasIPFS

         -- * AddrPart type
       , AddrPart(..)

         -- * IPv4 type
       , IPv4(..)
       , readIPv4
       , fromBytes

         -- * IPv6 type
       , IPv6(..)
       , readIPv6
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
data IPv4 = IPv4 {-# UNPACK #-} !Word32
          deriving (Eq, Ord, Bounded, Generic, Typeable)

instance Hashable IPv4

instance Serialize IPv4 where
  get = IPv4 <$> get
  put (IPv4 w) = put w

-- | An IPv6 address.
data IPv6 = IPv6
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
          deriving (Eq, Ord, Bounded, Generic, Typeable)

instance Hashable IPv6

instance Serialize IPv6 where
  get = IPv6 <$> get <*> get <*> get <*> get
  put (IPv6 a b c d) = put a >> put b >> put c >> put d

asBytes :: IPv4 -> [Word8]
asBytes (IPv4 ip) =
  map (\n -> fromIntegral (ip `shiftR` n) .&. 0xFF) [24, 16, 8, 0]

asPieces :: IPv6 -> [Word16]
asPieces (IPv6 a b c d) =
  map fromIntegral [ a `shiftR` 16
                   , a .&. 0xFFFF
                   , b `shiftR` 16
                   , b .&. 0xFFFF
                   , c `shiftR` 16
                   , c .&. 0xFFFF
                   , d `shiftR` 16
                   , d .&. 0xFFFF
                   ]

fromBytes :: (Word8, Word8, Word8, Word8) -> IPv4
fromBytes (a, b, c, d) = IPv4 ( shiftL (fromIntegral a) 24
                          `xor` shiftL (fromIntegral b) 16
                          `xor` shiftL (fromIntegral c) 8
                          `xor` fromIntegral d)

fromPieces :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
               -> IPv6
fromPieces (a, b, c, d, e, f, g, h) =
    IPv6 (word a b) (word c d) (word e f) (word g h)
  where word x y = (fromIntegral x `shiftL` 16) `xor` fromIntegral y

word8P :: Parser Word8
word8P = do
  w <- decimal :: Parser Integer
  guard (w >= 0 && w <= 255)
  pure (fromIntegral w)

ipv4P :: Parser IPv4
ipv4P = do
  a <- word8P <* char '.'
  b <- word8P <* char '.'
  c <- word8P <* char '.'
  d <- word8P
  pure (fromBytes (a, b, c, d))

-- | Try to read an IPv4 address (e.g. @"192.168.1.2"@)
readIPv4 :: Text -> Maybe IPv4
readIPv4 = hush . parseOnly (ipv4P <* endOfInput)

word16P :: Parser Word16
word16P = do
  w <- hexadecimal :: Parser Integer
  guard (w >= 0 && w <= 65535)
  pure (fromIntegral w)

ipv6P :: Parser IPv6
ipv6P = do
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
readIPv6 :: Text -> Maybe IPv6
readIPv6 = hush . parseOnly (ipv6P <* endOfInput)

ipv4B :: IPv4 -> Builder
ipv4B = mconcat . intersperse (singleton '.') . map Builder.decimal . asBytes

{- All we need for ipv6B to pass LiquidHaskell -}
{-@ assert scanl' :: (b -> a -> b) -> b -> xs:[a]
                     -> {v : [b] | len(v) = 1 + len(xs) } @-}

ipv6B :: IPv6 -> Builder
ipv6B ip =
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

instance TextAddr IPv4 where
  toText = toStrict . toLazyText . ipv4B

-- | RFC 5952 Compliant
instance TextAddr IPv6 where
  toText = toStrict . toLazyText . ipv6B

instance Show IPv4 where
  show = T.unpack . toText

instance Show IPv6 where
  show = T.unpack . toText

-- | An individual component of a multiaddr.
data AddrPart = IPv4Part !IPv4
              | IPv6Part !IPv6
              | UDPPart !Word16
              | TCPPart !Word16
              | DCCPPart !Word16
              | SCTPPart !Word16
              | IPFSPart !ByteString
              | UDTPart
              | UTPPart
              | HTTPPart
              | HTTPSPart
              deriving (Eq, Generic, Typeable)

instance Hashable AddrPart

instance Serialize AddrPart where
  put (IPv4Part ip) = put (4 :: Varint Word32) >> put ip
  put (IPv6Part ip) = put (41 :: Varint Word32) >> put ip
  put (UDPPart port) = put (17 :: Varint Word32) >> put port
  put (TCPPart port) = put (6 :: Varint Word32) >> put port
  put (DCCPPart port) = put (33 :: Varint Word32) >> put port
  put (SCTPPart port) = put (132 :: Varint Word32) >> put port
  put (IPFSPart addr) = do
    put (421 :: Varint Word32)
    case decodeBase58 bitcoinAlphabet addr of
      Nothing -> fail "invalid base58"
      Just bytes -> do
        put (fromIntegral (BS.length bytes) :: Varint Word64)
        mapM_ put (BS.unpack bytes)
  put UDTPart = put (301 :: Varint Word32)
  put UTPPart = put (302 :: Varint Word32)
  put HTTPPart = put (480 :: Varint Word32)
  put HTTPSPart = put (443 :: Varint Word32)

  get = do
    code <- get :: Get (Varint Word32)
    case code of
      4 -> IPv4Part <$> get
      6 -> TCPPart <$> get
      17 -> UDPPart <$> get
      33 -> DCCPPart <$> get
      41 -> IPv6Part <$> get
      132 -> SCTPPart <$> get
      421 -> do
        len <- get :: Get (Varint Word64)
        bytes <- getBytes (fromIntegral len)
        let base58 = encodeBase58 bitcoinAlphabet bytes
        pure (IPFSPart base58)
      301 -> pure UDTPart
      302 -> pure UTPPart
      480 -> pure HTTPPart
      443 -> pure HTTPSPart
      _ -> fail "invalid multiaddr code"

-- | A network address.
newtype Multiaddr = Multiaddr { _parts :: [AddrPart] }
                  deriving (Eq, Monoid, Generic, Typeable)

instance Hashable Multiaddr

instance Serialize Multiaddr where
  get = Multiaddr <$> many get

  put (Multiaddr ps) = mapM_ put ps

-- | Get the individual parts of the multiaddr, in order
-- (e.g. @["\/ip4\/8.8.8.8", "\/tcp\/80"]@)
parts :: Multiaddr -> [AddrPart]
parts = _parts

ipv4PartP :: Parser AddrPart
ipv4PartP = IPv4Part <$> (string "/ip4/" *> ipv4P)

ipv6PartP :: Parser AddrPart
ipv6PartP = IPv6Part <$> (string "/ip6/" *> ipv6P)

udpPartP :: Parser AddrPart
udpPartP = do
  _ <- string "/udp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (UDPPart (fromIntegral port))

dccpPartP :: Parser AddrPart
dccpPartP = do
  _ <- string "/dccp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (DCCPPart (fromIntegral port))

sctpPartP :: Parser AddrPart
sctpPartP = do
  _ <- string "/sctp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (SCTPPart (fromIntegral port))

udtPartP :: Parser AddrPart
udtPartP = string "/udt" >> pure UDTPart

utpPartP :: Parser AddrPart
utpPartP = string "/utp" >> pure UTPPart

httpPartP :: Parser AddrPart
httpPartP = string "/http" >> pure HTTPPart

httpsPartP :: Parser AddrPart
httpsPartP = string "/https" >> pure HTTPSPart

tcpPartP :: Parser AddrPart
tcpPartP = do
  _ <- string "/tcp/"
  port <- decimal :: Parser Integer
  guard (port >= 0 && port <= 65535)
  pure (TCPPart (fromIntegral port))

ipfsPartP :: Parser AddrPart
ipfsPartP = do
  _ <- string "/ipfs/"
  addr <- takeTill (== '/')
  pure (IPFSPart (T.encodeUtf8 addr))

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

addrPartB :: AddrPart -> Builder
addrPartB (IPv4Part ip) = fromText "/ip4/" <> ipv4B ip
addrPartB (IPv6Part ip) = fromText "/ip6/" <> ipv6B ip
addrPartB (UDPPart port) = fromText "/udp/" <> Builder.decimal port
addrPartB (TCPPart port) = fromText "/tcp/" <> Builder.decimal port
addrPartB (DCCPPart port) = fromText "/dccp/" <> Builder.decimal port
addrPartB (SCTPPart port) = fromText "/sctp/" <> Builder.decimal port
addrPartB (IPFSPart addr) = fromText "/ipfs/" <> fromText (T.decodeUtf8 addr)
addrPartB UDTPart = fromText "/udt"
addrPartB UTPPart = fromText "/utp"
addrPartB HTTPPart = fromText "/http"
addrPartB HTTPSPart = fromText "/https"

instance TextAddr AddrPart where
  toText = toStrict . toLazyText . addrPartB

instance Show AddrPart where
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
hasIPv4 (Multiaddr ps) = any (\case IPv4Part _ -> True; _ -> False) ps

-- | Does the multiaddr contain an IPv6 part?
hasIPv6 :: Multiaddr -> Bool
hasIPv6 (Multiaddr ps) = any (\case IPv6Part _ -> True; _ -> False) ps

-- | Does the multiaddr contain a TCP part?
hasTCP :: Multiaddr -> Bool
hasTCP (Multiaddr ps) = any (\case TCPPart _ -> True; _ -> False) ps

-- | Does the multiaddr contain a UDP part?
hasUDP :: Multiaddr -> Bool
hasUDP (Multiaddr ps) = any (\case UDPPart _ -> True; _ -> False) ps

-- | Does the multiaddr contain a DCCP part?
hasDCCP :: Multiaddr -> Bool
hasDCCP (Multiaddr ps) = any (\case DCCPPart _ -> True; _ -> False) ps

-- | Does the multiaddr contain an SCTP part?
hasSCTP :: Multiaddr -> Bool
hasSCTP (Multiaddr ps) = any (\case SCTPPart _ -> True; _ -> False) ps

-- | Does the multiaddr contain an IPFS part?
hasIPFS :: Multiaddr -> Bool
hasIPFS (Multiaddr ps) = any (\case IPFSPart _ -> True; _ -> False) ps

-- | Append two multiaddrs (alias for @(<>)@)
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate = mappend

-- | Get an ordered list of the protocols specified by the multiaddr.
-- Protocols can appear more than once if they are repeated in the multiaddr.
protocolNames :: Multiaddr -> [Text]
protocolNames (Multiaddr ms) = map protoName ms
  where protoName (IPv4Part _) = "ip4"
        protoName (IPv6Part _) = "ip6"
        protoName (UDPPart _) = "udp"
        protoName (TCPPart _) = "tcp"
        protoName (DCCPPart _) = "dccp"
        protoName (SCTPPart _) = "sctp"
        protoName (IPFSPart _) = "ipfs"
        protoName UDTPart = "udt"
        protoName UTPPart = "utp"
        protoName HTTPPart = "http"
        protoName HTTPSPart = "https"
