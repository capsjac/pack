-- |
-- Module      : Data.Pack.Endianness
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
{-# LANGUAGE CPP #-}
module Data.Pack.Endianness
  ( le16Host
  , le32Host
  , le64Host
  , be16Host
  , be32Host
  , be64Host
  ) where

import Data.Bits
import Data.Word

#if MIN_VERSION_base(4,7,0)
-- | Swap endianness on a Word16.
swap16 :: Word16 -> Word16
swap16 = byteSwap16

-- | Swap endianness on a Word32.
swap32 :: Word32 -> Word32
swap32 = byteSwap32

-- | Swap endianness on a Word64.
swap64 :: Word64 -> Word64
swap64 = byteSwap64

#else

#if BITS_IS_OLD
shr :: Bits a => a -> Int -> a
shr = shiftR
shl :: Bits a => a -> Int -> a
shl = shiftL
#else
shr :: Bits a => a -> Int -> a
shr = unsafeShiftR
shl :: Bits a => a -> Int -> a
shl = unsafeShiftL
#endif

-- | Swap endianness on a Word64.
-- 56 48 40 32 24 16  8  0
--  a  b  c  d  e  f  g  h 
--  h  g  f  e  d  c  b  a
swap64 :: Word64 -> Word64
swap64 w =
      (w `shr` 56)                  .|. (w `shl` 56)
  .|. ((w `shr` 40) .&. 0xff00)     .|. ((w .&. 0xff00) `shl` 40)
  .|. ((w `shr` 24) .&. 0xff0000)   .|. ((w .&. 0xff0000) `shl` 24)
  .|. ((w `shr` 8)  .&. 0xff000000) .|. ((w .&. 0xff000000) `shl` 8)

-- | Swap endianness on a Word32.
swap32 :: Word32 -> Word32
swap32 w =
      (w `shr` 24)             .|. (w `shl` 24)
  .|. ((w `shr` 8) .&. 0xff00) .|. ((w .&. 0xff00) `shl` 8)

-- | Swap endianness on a Word16.
swap16 :: Word16 -> Word16
swap16 w = (w `shr` 8) .|. (w `shl` 8)

#endif

#ifdef CPU_BIG_ENDIAN

#define FromBE(bits) id
#define FromLE(bits) swap/**/bits

#else

#define FromBE(bits) swap/**/bits
#define FromLE(bits) id

#endif

-- | 16 bit big endian to host endian.
{-# INLINE be16Host #-}
be16Host :: Word16 -> Word16
be16Host = FromBE(16)

-- | 32 bit big endian to host endian.
{-# INLINE be32Host #-}
be32Host :: Word32 -> Word32
be32Host = FromBE(32)

-- | 64 bit big endian to host endian.
{-# INLINE be64Host #-}
be64Host :: Word64 -> Word64
be64Host = FromBE(64)

-- | 16 bit little endian to host endian.
le16Host :: Word16 -> Word16
le16Host = FromLE(16)

-- | 32 bit little endian to host endian.
le32Host :: Word32 -> Word32
le32Host = FromLE(32)

-- | 64 bit little endian to host endian.
le64Host :: Word64 -> Word64
le64Host = FromLE(64)

