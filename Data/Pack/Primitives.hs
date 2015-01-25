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
  ) where

import Control.Applicative
import Data.Pack.Endianness
import Data.Pack.IEEE754
import Foreign


type Packer a = a -> Packet String a

newtype Packet e a = Packet
	{ unPacket ::
		( Ptr () -> Ptr () -> Ptr () -> IO (Ptr (), Either e a)
		, Int -> Int
		, Ptr () -> Ptr () -> Ptr () -> IO (Ptr ()) )
	}

i8 :: Packer Int8
i8 = simple 1 id id
{-# INLINE i8 #-}

i16 :: Packer Int16
i16 = simple 2 (fromIntegral . le16Host) (le16Host . fromIntegral)
{-# INLINE i16 #-}

i16host :: Packer Int16
i16host = simple 2 id id
{-# INLINE i16host #-}

i16be :: Packer Int16
i16be = simple 2 (fromIntegral . be16Host) (be16Host . fromIntegral)
{-# INLINE i16be #-}

i32 :: Packer Int32
i32 = simple 4 (fromIntegral . le32Host) (le32Host . fromIntegral)
{-# INLINE i32 #-}

i32host :: Packer Int32
i32host = simple 4 id id
{-# INLINE i32host #-}

i32be :: Packer Int32
i32be = simple 4 (fromIntegral . be32Host) (be32Host . fromIntegral)
{-# INLINE i32be #-}

i64 :: Packer Int64
i64 = simple 8 (fromIntegral . le64Host) (le64Host . fromIntegral)
{-# INLINE i64 #-}

i64host :: Packer Int64
i64host = simple 8 id id
{-# INLINE i64host #-}

i64be :: Packer Int64
i64be = simple 8 (fromIntegral . be64Host) (be64Host . fromIntegral)
{-# INLINE i64be #-}

u8 :: Packer Word8
u8 = simple 1 id id
{-# INLINE u8 #-}

u16 :: Packer Word16
u16 = simple 2 le16Host le16Host
{-# INLINE u16 #-}

u16host :: Packer Word16
u16host = simple 2 id id
{-# INLINE u16host #-}

u16be :: Packer Word16
u16be = simple 2 be16Host be16Host
{-# INLINE u16be #-}

u32 :: Packer Word32
u32 = simple 4 le32Host le32Host
{-# INLINE u32 #-}

u32host :: Packer Word32
u32host = simple 4 id id
{-# INLINE u32host #-}

u32be :: Packer Word32
u32be = simple 4 be32Host be32Host
{-# INLINE u32be #-}

u64 :: Packer Word64
u64 = simple 8 le64Host le64Host
{-# INLINE u64 #-}

u64host :: Packer Word64
u64host = simple 8 id id
{-# INLINE u64host #-}

u64be :: Packer Word64
u64be = simple 8 be64Host be64Host
{-# INLINE u64be #-}


f32 :: Packer Float
f32 = simple 4 wordToFloat floatToWord
{-# INLINE f32 #-}

f64 :: Packer Double
f64 = simple 8 wordToDouble doubleToWord
{-# INLINE f64 #-}



simple :: Storable a => Int -> (a -> b) -> (b -> a) -> Packer b
simple n toHost fromHost = \a ->
	Packet (peekWith n peek toHost, (+n), pokeWith n poke (fromHost a))
{-# INLINE simple #-}

peekWith :: Int -> (Ptr a -> IO a) -> (a -> b) -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr (), Either String b)
peekWith n f toHost _ bottom ptr | plusPtr ptr n <= bottom =
	(plusPtr ptr n,) . Right . toHost <$> f (castPtr ptr)
peekWith _ _ _ _ _ ptr =
	return (ptr, Left "not enough bytes.")
{-# INLINE peekWith #-}

pokeWith :: Int -> (Ptr a -> a -> IO ()) -> a -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
pokeWith n f a = \_ _ p -> f (castPtr p) a >> return (plusPtr p n)
{-# INLINE pokeWith #-}

