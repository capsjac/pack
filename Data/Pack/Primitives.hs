-- |
-- Module      : Data.Pack.Primitives
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Portable
--

module Data.Pack.Primitives
  (
  -- * Fixed sized
    i8
  , i16
  , i32
  , i64
  , u8
  , u16
  , u32
  , u64
  , i16b
  , i32b
  , i64b
  , u16b
  , u32b
  , u64b
  , i16host
  , i32host
  , i64host
  , iptrsize
  , u16host
  , u32host
  , u64host
  , uptrsize
  , f32
  , f64
  , f32b
  , f64b
  , f32host
  , f64host
  ) where

import Data.Pack.Endianness
import Data.Pack.IEEE754
import Data.Pack.Packet
import Foreign


-- | A 'Int8' 'Packet'.
i8 :: Packer Int8
i8 = simple 1 id id
{-# INLINE i8 #-}

-- | A 'Int16' 'Packet' serialized in little endian.
i16 :: Packer Int16
i16 = simple 2 (fromIntegral . le16Host) (le16Host . fromIntegral)
{-# INLINE i16 #-}

-- | A 'Int16' 'Packet' in the host endianness.
i16host :: Packer Int16
i16host = simple 2 id id
{-# INLINE i16host #-}

-- | A 'Int16' 'Packet' serialized in big endian.
i16b :: Packer Int16
i16b = simple 2 (fromIntegral . be16Host) (be16Host . fromIntegral)
{-# INLINE i16b #-}

-- | A 'Int32' 'Packet' serialized in little endian.
i32 :: Packer Int32
i32 = simple 4 (fromIntegral . le32Host) (le32Host . fromIntegral)
{-# INLINE i32 #-}

-- | A 'Int32' 'Packet' in the host endianness.
i32host :: Packer Int32
i32host = simple 4 id id
{-# INLINE i32host #-}

-- | A 'Int32' 'Packet' serialized in big endian.
i32b :: Packer Int32
i32b = simple 4 (fromIntegral . be32Host) (be32Host . fromIntegral)
{-# INLINE i32b #-}

-- | A 'Int64' 'Packet' serialized in little endian.
i64 :: Packer Int64
i64 = simple 8 (fromIntegral . le64Host) (le64Host . fromIntegral)
{-# INLINE i64 #-}

-- | A 'Int64' 'Packet' in the host endianness.
i64host :: Packer Int64
i64host = simple 8 id id
{-# INLINE i64host #-}

-- | A 'Int64' 'Packet' serialized in big endian.
i64b :: Packer Int64
i64b = simple 8 (fromIntegral . be64Host) (be64Host . fromIntegral)
{-# INLINE i64b #-}

-- | A host pointer-sized 'Int' 'Packet' in the host endianness.
iptrsize :: Packer Word
iptrsize a = simple (sizeOf a) id id a
{-# INLINE iptrsize #-}

-- | A 'Word8' 'Packet'.
u8 :: Packer Word8
u8 = simple 1 id id
{-# INLINE u8 #-}

-- | A 'Word16' 'Packet' serialized in little endian.
u16 :: Packer Word16
u16 = simple 2 le16Host le16Host
{-# INLINE u16 #-}

-- | A 'Word16' 'Packet' in the host endianness.
u16host :: Packer Word16
u16host = simple 2 id id
{-# INLINE u16host #-}

-- | A 'Word16' 'Packet' serialized in big endian.
u16b :: Packer Word16
u16b = simple 2 be16Host be16Host
{-# INLINE u16b #-}

-- | A 'Word32' 'Packet' serialized in little endian.
u32 :: Packer Word32
u32 = simple 4 le32Host le32Host
{-# INLINE u32 #-}

-- | A 'Word32' 'Packet' in the host endianness.
u32host :: Packer Word32
u32host = simple 4 id id
{-# INLINE u32host #-}

-- | A 'Word32' 'Packet' serialized in big endian.
u32b :: Packer Word32
u32b = simple 4 be32Host be32Host
{-# INLINE u32b #-}

-- | A 'Word64' 'Packet' serialized in little endian.
u64 :: Packer Word64
u64 = simple 8 le64Host le64Host
{-# INLINE u64 #-}

-- | A 'Word64' 'Packet' in the host endianness.
u64host :: Packer Word64
u64host = simple 8 id id
{-# INLINE u64host #-}

-- | A 'Word64' 'Packet' serialized in big endian.
u64b :: Packer Word64
u64b = simple 8 be64Host be64Host
{-# INLINE u64b #-}

-- | A host pointer-sized 'Word' 'Packet' in the host endianness.
uptrsize :: Packer Word
uptrsize a = simple (sizeOf a) id id a
{-# INLINE uptrsize #-}

-- | A IEEE754-'Float' 'Packet' serialized in little endian.
f32 :: Packer Float
f32 = simple 4 (wordToFloat.le32Host) (le32Host.floatToWord)
{-# INLINE f32 #-}

-- | A IEEE754-'Double' 'Packet' serialized in little endian.
f64 :: Packer Double
f64 = simple 8 (wordToDouble.le64Host) (le64Host.doubleToWord)
{-# INLINE f64 #-}

-- | A IEEE754-'Float' 'Packet' in the host endianness.
f32host :: Packer Float
f32host = simple 4 wordToFloat floatToWord
{-# INLINE f32host #-}

-- | A IEEE754-'Double' 'Packet' in the host endianness.
f64host :: Packer Double
f64host = simple 8 wordToDouble doubleToWord
{-# INLINE f64host #-}

-- | A IEEE754-'Float' 'Packet' serialized in big endian.
f32b :: Packer Float
f32b = simple 4 (wordToFloat.be32Host) (be32Host.floatToWord)
{-# INLINE f32b #-}

-- | A IEEE754-'Double' 'Packet' serialized in big endian.
f64b :: Packer Double
f64b = simple 8 (wordToDouble.be64Host) (be64Host.doubleToWord)
{-# INLINE f64b #-}

