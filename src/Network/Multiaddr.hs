{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Multiaddr
       ( IPv4(..)
       , IPv6(..)
       , TextIP(..)
       , readIPv4
       , readIPv6
       , fromBytes
       , fromPieces
       ) where

import           Control.Monad (guard)
import           Data.Bits
import           Data.List (intersperse, scanl', elemIndex)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Word (Word32, Word16, Word8)

import           Control.Error (hush)
import           Data.Attoparsec.Text hiding (take)
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, toLazyText, singleton,
                                         fromText)
import qualified Data.Text.Lazy.Builder.Int as Builder

data IPv4 = IPv4 {-# UNPACK #-} !Word32
          deriving (Eq, Ord, Bounded, Generic, Typeable)

instance Hashable IPv4

data IPv6 = IPv6
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
            {-# UNPACK #-} !Word32
          deriving (Eq, Ord, Bounded, Generic, Typeable)

instance Hashable IPv6

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

class TextIP a where
  toText :: a -> Text

instance TextIP IPv4 where
  toText = toStrict . toLazyText . ipv4B

-- | RFC 5952 Compliant
instance TextIP IPv6 where
  toText = toStrict . toLazyText . ipv6B

instance Show IPv4 where
  show = T.unpack . toText

instance Show IPv6 where
  show = T.unpack . toText
