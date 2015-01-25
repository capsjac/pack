-- |
-- Module      : Data.Pack.Primitives
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
{-# LANGUAGE TupleSections #-}

module Data.Pack.Primitives
  ( Packer
  , Packet(..)
  , i8
  , i16
  , i32
  , i64
  , u8
  , u16
  , u32
  , u64
  , i16host
  , i32host
  , i64host
  , u16host
  , u32host
  , u64host
  , i16be
  , i32be
  , i64be
  , u16be
  , u32be
  , u64be
  , f32
  , f64
  , f32host
  , f64host
  , f32be
  , f64be
  , bytes
  , bytesCopy
  --, bytesWhile
  --, remainingBytes
  --, remainingBytesCopy
  , storable
  --, extensible
  --, isolate
  --, anchor
  --, vector
  --, vector'
  , pad
  , unused
  --, align
  --, align0
  --, getPosition
  --, getTotalSize
  --, getRemaining
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString as B (ByteString, copy)
import qualified Data.ByteString.Internal as B
import Data.Pack.Endianness
import Data.Pack.IEEE754
import Data.Pack.Types
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
i16be :: Packer Int16
i16be = simple 2 (fromIntegral . be16Host) (be16Host . fromIntegral)
{-# INLINE i16be #-}

-- | A 'Int32' 'Packet' serialized in little endian.
i32 :: Packer Int32
i32 = simple 4 (fromIntegral . le32Host) (le32Host . fromIntegral)
{-# INLINE i32 #-}

-- | A 'Int32' 'Packet' in the host endianness.
i32host :: Packer Int32
i32host = simple 4 id id
{-# INLINE i32host #-}

-- | A 'Int32' 'Packet' serialized in big endian.
i32be :: Packer Int32
i32be = simple 4 (fromIntegral . be32Host) (be32Host . fromIntegral)
{-# INLINE i32be #-}

-- | A 'Int64' 'Packet' serialized in little endian.
i64 :: Packer Int64
i64 = simple 8 (fromIntegral . le64Host) (le64Host . fromIntegral)
{-# INLINE i64 #-}

-- | A 'Int64' 'Packet' in the host endianness.
i64host :: Packer Int64
i64host = simple 8 id id
{-# INLINE i64host #-}

-- | A 'Int64' 'Packet' serialized in big endian.
i64be :: Packer Int64
i64be = simple 8 (fromIntegral . be64Host) (be64Host . fromIntegral)
{-# INLINE i64be #-}

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
u16be :: Packer Word16
u16be = simple 2 be16Host be16Host
{-# INLINE u16be #-}

-- | A 'Word32' 'Packet' serialized in little endian.
u32 :: Packer Word32
u32 = simple 4 le32Host le32Host
{-# INLINE u32 #-}

-- | A 'Word32' 'Packet' in the host endianness.
u32host :: Packer Word32
u32host = simple 4 id id
{-# INLINE u32host #-}

-- | A 'Word32' 'Packet' serialized in big endian.
u32be :: Packer Word32
u32be = simple 4 be32Host be32Host
{-# INLINE u32be #-}

-- | A 'Word64' 'Packet' serialized in little endian.
u64 :: Packer Word64
u64 = simple 8 le64Host le64Host
{-# INLINE u64 #-}

-- | A 'Word64' 'Packet' in the host endianness.
u64host :: Packer Word64
u64host = simple 8 id id
{-# INLINE u64host #-}

-- | A 'Word64' 'Packet' serialized in big endian.
u64be :: Packer Word64
u64be = simple 8 be64Host be64Host
{-# INLINE u64be #-}

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
f32be :: Packer Float
f32be = simple 4 (wordToFloat.be32Host) (be32Host.floatToWord)
{-# INLINE f32be #-}

-- | A IEEE754-'Double' 'Packet' serialized in big endian.
f64be :: Packer Double
f64be = simple 8 (wordToDouble.be64Host) (be64Host.doubleToWord)
{-# INLINE f64be #-}

-- | A strict 'ByteString' 'Packet'.
bytes :: Int -> Packer ByteString
bytes n = \bs -> Packet
  ( \fp btm ptr -> do
      let ptr' = plusPtr ptr n
      r <- --if ptr' <= btm
      --  then Right 
      --              o <- withForeignPtr fp $ \origPtr -> return (ptr `minusPtr` origPtr)
      --              return $ B.fromForeignPtr fp offset n
      --  else
        return $ Left "not enough bytes." 
      return (ptr', r)
  , (+n)
  , \_ _ ptr -> do
      let (fp, off, len) = B.toForeignPtr bs
      withForeignPtr fp $ \ptr' ->
        B.memcpy (castPtr ptr) (ptr' `plusPtr` off) (fromIntegral len)
      return (plusPtr ptr n)
  )

-- | A strict 'ByteString' 'Packet'.
bytesCopy :: Int -> Packer ByteString
bytesCopy n = \bs ->
  case bytes n bs of
    Packet (get, size, put) -> Packet
      (\t b p -> fmap (fmap B.copy) <$> get t b p, size, put)

-- | A 'Storable' 'Packet'.
storable :: Storable a => Packer a
storable a = simple (sizeOf a) id id a
{-# INLINE storable #-}

-- | Skip bytes, filling with specified byte.
pad :: Word8 -> Int -> Packet String ()
pad filler n = replicateM_ n (u8 filler) -- XXX make this faster
{-# INLINE pad #-}

-- | Skip bytes, filling with NUL bytes.
unused :: Int -> Packet String ()
unused = pad 0
{-# INLINE unused #-}

simple :: Storable a => Int -> (a -> b) -> (b -> a) -> Packer b
simple n toHost fromHost = \a ->
	Packet (peekWith n peek toHost, (+n), pokeWith n poke (fromHost a))
{-# INLINE simple #-}

peekWith :: Int -> (Ptr a -> IO a) -> (a -> b) -> ForeignPtr Word8 -> Ptr () -> Ptr () -> IO (Ptr (), Either String b)
peekWith n f toHost _ bottom ptr | plusPtr ptr n <= bottom =
	(plusPtr ptr n,) . Right . toHost <$> f (castPtr ptr)
peekWith _ _ _ _ _ ptr =
	return (ptr, Left "not enough bytes.")
{-# INLINE peekWith #-}

pokeWith :: Int -> (Ptr a -> a -> IO ()) -> a -> ForeignPtr Word8 -> Ptr () -> Ptr () -> IO (Ptr ())
pokeWith n f a = \_ _ p -> f (castPtr p) a >> return (plusPtr p n)
{-# INLINE pokeWith #-}

