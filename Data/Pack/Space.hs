-- |
-- Module      : Data.Pack.Space
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Portable
--

module Data.Pack.Space
  (
  -- * Spaces
    unused
  , pad
  , alignedTo
  , alignedWith
  , alignPos
  , alignLen
  ) where

import Control.Monad (replicateM_)
import Data.Pack.Packet (Packet, simple)
import Data.Pack.Utils (getPosition)
import Data.Word (Word8)


-- | Skip bytes, filling with NUL bytes.
unused :: Num a => a -> Packet String ()
unused = pad 0
{-# INLINE unused #-}

-- | Skip bytes, filling with specified byte.
pad :: Num a => Word8 -> a -> Packet String ()
pad filler n = replicateM_ (fromIntegral n) (simple 1 id id filler) -- XXX make this faster
{-# INLINE pad #-}

-- | (Unpacker only) Adjust alignment, filling with NUL bytes.
alignedTo :: Num a => a -> Packet String ()
alignedTo = alignedWith 0
{-# INLINE alignedTo #-}

-- | (Unpacker only) Adjust alignment, filling with specified byte.
alignedWith :: Num a => Word8 -> a -> Packet String ()
alignedWith filler blockSize = do
  pos <- getPosition
  pad filler (alignLen blockSize pos)
{-# INLINE alignedWith #-}

-- | Round up byte-offset to be aligned by the value specified in the first argument.
alignPos :: (Num a, Bits a) => a -> a -> a
alignPos alignment n = (n + alignment - 1) .&. complement (alignment - 1)
{-# INLINE alignPos #-}

-- | A number of padding bytes required to match alignment specified in the first argument.
alignLen :: (Num a, Bits a) => a -> a -> a
alignLen alignment pos = pos - alignedSize alignment pos
{-# INLINE alignLen #-}

