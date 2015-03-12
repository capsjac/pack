-- |
-- Module      : Data.Pack.Structure
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Portable
--

module Data.Pack.Structure
  (
  -- * Structures
    vector
  , array
  , arrayCopy
  , storable
  --, extensible
  , enumOf
  --, bitfields :: Packet a -> (a -> b) -> Packet b
  , dimapP
  , dicase
  --, isolate :: Packer a -> Packer a
  --, hole :: Packer a -> a -> Packet (Packet a)
  --, readAhead :: Packer a -> Packer a
  ) where

import Data.ByteString.Internal (ByteString(..))
import Data.Pack.Packet
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Internal (getPtr)
import Foreign


-- | A "Data.Vector.Generic" 'Packet'. Get and put an array of arbitary
-- type of 'Packet's.
vector :: (V.Vector v a, Num n) => Packer a -> n -> Packer (v a)
vector packer n vec = Packet (get, size, put)
  where
    Packet (get, _, _) = V.replicateM (fromIntegral n) (packer undefined)
    Packet (_, size, put) = V.mapM packer vec
{-# INLINE vector #-}

-- | A "Data.Vector.Storable" 'Packet'. Read operation is copy-free.
array :: (Storable a, Num n) => n -> Packer (VS.Vector a)
array count vec = asymmPacket get put (V.length vec * sizeOfA)
  where
    get (PS fptr offset len) = return (reqSize, result)
      where
        fp = castForeignPtr fptr
        reqSize = (fromIntegral count) * sizeOfA
        result = if reqSize <= len
          then Right $ VS.unsafeFromForeignPtr fp offset (fromIntegral count)
          else Left "not enough bytes to obtain array."
    put dstPtr = VS.unsafeWith vec $ \srcPtr ->
      copyArray (castPtr dstPtr) srcPtr (V.length vec)
    sizeOfA = sizeOf (V.head vec)
{-# INLINE array #-}

-- | Similar to 'array' but copy the bytes.
arrayCopy :: (Storable a, Num n) => n -> Packer (VS.Vector a)
arrayCopy n = fmap VS.force . array n
{-# INLINE arrayCopy #-}

-- | A 'Storable' 'Packet'.
-- Note that 'Storable' must not be used to store variable sized structures.
storable :: Storable a => Packer a
storable a = simple (sizeOf a) id id a
{-# INLINE storable #-}

-- | Represent a simple enum field.
enumOf :: (Integral a, Enum b) => Packer a -> Packer b
enumOf = dimapP (fromIntegral.fromEnum) (toEnum.fromIntegral)
{-# INLINE enumOf #-}

-- | A dirty hack to handle unions.
-- 
-- > tag <- i32 (getTagId dat)
-- > let getcase tag = case tag of
-- >   0 -> A <$> i32 undefined
-- >   1 -> B <$> f32 undefined
-- > let putcase dat = case dat of
-- >   A i -> i32 i
-- >   B f -> f32 f
-- > val <- dicase (getcase tag) (putcase data)
dicase :: Packet e a -> Packet e' a -> Packet e a
dicase getcase@(Packet (get, _, _))
       putcase@(Packet (_, size, put)) =
  Packet (get, size, put)
{-# INLINE dicase #-}

--  | peek/poke helpers for writing 'Storable' instances.
-- Note that 'Storable' must not be used to store variable sized structures.
-- peekPacket :: Packer a -> Ptr a -> IO a
-- pokePacket :: Packer a -> Ptr a -> a -> IO ()
-- isFull->isEmpty, diif, direturn, bimonad, birif, rif, lif to bifunctors

