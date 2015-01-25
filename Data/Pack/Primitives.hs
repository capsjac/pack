{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Data.Pack.Primitives
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
module Data.Pack.Primitives
  
   where

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
i8 a = Packet (needs 1 peek, (+1), pokeWith 1 poke a)
{-# INLINE i8 #-}

i16 :: Packer Int16
i16 = simple 2 peek poke
{-# INLINE i16 #-}

i16host :: Packer Int16
i16host = simple 2 peek poke
{-# INLINE i16host #-}

i32 :: Packer Int32
i32 = simple 4 peek poke
{-# INLINE i32 #-}

i64 :: Packer Int64
i64 = simple 8 peek poke
{-# INLINE i64 #-}

u8 :: Packer Word8
u8 = simple 1 peek poke
{-# INLINE u8 #-}

u16 :: Packer Word16
u16 = simple 2 peek poke
{-# INLINE u16 #-}

u32 :: Packer Word32
u32 = simple 4 peek poke
{-# INLINE u32 #-}

u64 :: Packer Word64
u64 = simple 8 peek poke
{-# INLINE u64 #-}


f32 :: Packer Float
f32 = simple 4 (wordToFloat <$> peek) (poke . floatToWord)
{-# INLINE f32 #-}

f64 :: Packer Double
f64 = simple 8 (wordToDouble <$> peek) (poke . doubleToWord)
{-# INLINE f64 #-}



simple :: Int -> (Ptr a -> IO a) -> (Ptr a -> a -> IO ()) -> Packer a
simple n get put = \a ->
	Packet (needs n get, (+n), pokeWith n put a)
{-# INLINE simple #-}

needs :: Int -> (Ptr a -> IO a) -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr (), Either String a)
needs n f _ bottom ptr | plusPtr ptr n <= bottom =
	(plusPtr ptr n,) . Right <$> f (castPtr ptr)
needs _ _ _ _ ptr =
	return (ptr, Left "not enough bytes.")
{-# INLINE needs #-}

pokeWith :: Int -> (Ptr a -> a -> IO ()) -> a -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
pokeWith n f a = \_ _ p -> f (castPtr p) a >> return (plusPtr p 1)
{-# INLINE pokeWith #-}

