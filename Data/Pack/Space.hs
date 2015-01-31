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
  ) where

import Control.Monad (replicateM_)
import Data.Pack.Packet (Packet, simple)
import Data.Pack.Utils (getPosition)
import Data.Word (Word8)


-- | Skip bytes, filling with NUL bytes.
unused :: Int -> Packet String ()
unused = pad 0
{-# INLINE unused #-}

-- | Skip bytes, filling with specified byte.
pad :: Word8 -> Int -> Packet String ()
pad filler n = replicateM_ n (simple 1 id id filler) -- XXX make this faster
{-# INLINE pad #-}

-- | Adjust alignment, filling with NUL bytes.
alignedTo :: Int -> Packet String ()
alignedTo = alignedWith 0
{-# INLINE alignedTo #-}

-- | Adjust alignment, filling with specified byte.
alignedWith :: Word8 -> Int -> Packet String ()
alignedWith filler blockSize = do
  pos <- getPosition
  pad filler (mod pos blockSize)
{-# INLINE alignedWith #-}

--aligned :: Int -> Int -> Int
--aligned alignment n = (n + alignment - 1) .&. complement (alignment - 1)

