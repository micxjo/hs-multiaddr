{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Serialize.Varint
       ( Varint(..)
       , getVarint
       , putVarint
       ) where

import Data.Bits
import GHC.Word (Word8)

import Data.Serialize (Serialize(..), Get, Put, putWord8, getWord8)

newtype Varint a = Varint a
                 deriving (Eq, Num, Ord, Real, Integral, Enum, Show, Bits)

getVarint :: (Num a, Bits a) => Word8 -> Get (Varint a)
getVarint n
  | testBit n 7 = do
      Varint m <- getWord8 >>= getVarint
      pure (shiftL m 7 .|. clearBit (fromIntegral n) 7)
  | otherwise = pure (fromIntegral n)


putVarint :: (Integral a, Bits a) => a -> Put
putVarint n
  | n < 0x80 = putWord8 (fromIntegral n)
  | otherwise = do
      putWord8 (setBit (fromIntegral n) 7)
      putVarint (shiftR n 7)

instance (Bits a, Integral a) => Serialize (Varint a) where
  get = getWord8 >>= getVarint
  put = putVarint
