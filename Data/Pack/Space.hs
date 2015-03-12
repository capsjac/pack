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
import Data.Bits (Bits, (.&.), complement)
import Data.Pack.Packet (Packet, simple)
import Data.Pack.Utils (getPosition)
import Data.Word (Word8)


-- | Skip bytes, filling with NUL bytes.
unused :: Integral a => a -> Packet String ()
unused = pad 0
{-# INLINE unused #-}

-- | Skip bytes, filling with specified byte.
pad :: Integral a => Word8 -> a -> Packet String ()
pad filler n = replicateM_ (fromIntegral n) (simple 1 id id filler) -- XXX make this faster
{-# INLINE pad #-}

-- | (Unpacker only) Adjust alignment, filling with NUL bytes.
alignedTo :: Integral a => a -> Packet String ()
alignedTo = alignedWith 0
{-# INLINE alignedTo #-}

-- | (Unpacker only) Adjust alignment, filling with specified byte.
alignedWith :: Integral a => Word8 -> a -> Packet String ()
alignedWith filler blockSize = do
  pos <- getPosition
  pad filler (alignLen (fromIntegral blockSize) pos)
{-# INLINE alignedWith #-}

-- | Round up byte-offset to be aligned by the value specified in the first argument.
alignPos :: (Integral a, Bits a) => a -> a -> a
alignPos alignment n = (n + alignment - 1) .&. complement (alignment - 1)
{-# INLINE alignPos #-}

-- | A number of padding bytes required to match alignment specified in the first argument.
alignLen :: (Integral a, Bits a) => a -> a -> a
alignLen alignment pos = alignPos alignment pos - pos
{-# INLINE alignLen #-}

