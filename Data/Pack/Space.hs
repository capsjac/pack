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
  --, skipWhile

  -- * Information
  , getPosition
  , getTotalSize
  , getRemaining
  , isFull
  --, sizeOfPacket
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString
import Data.Pack.Types
import Data.Word
import Foreign.Ptr


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

getPosition :: Packet e Int
getPosition = mkInfoPacket $ \bs _ cur -> do
  top <- getTop bs
  return (cur, Right (cur `minusPtr` top))
{-# INLINE getPosition #-}

getTotalSize :: Packet e Int
getTotalSize = mkInfoPacket $ \bs bottom cur -> do
  top <- getTop bs
  return (cur, Right (bottom `minusPtr` top))
{-# INLINE getTotalSize #-}

getRemaining :: Packet e Int
getRemaining = mkInfoPacket $ \_ bottom cur ->
  return (cur, Right (bottom `minusPtr` cur))
{-# INLINE getRemaining #-}

isFull :: Packet e Bool
isFull = (== 0) <$> getRemaining
{-# INLINE isFull #-}

mkInfoPacket :: (ByteString -> Ptr () -> Ptr () -> IO (Ptr (), Either e a)) -> Packet e a
mkInfoPacket f = Packet
  (f, id, \_ _ p -> return p )
{-# INLINE mkInfoPacket #-}

