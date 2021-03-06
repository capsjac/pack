-- |
-- Module      : Data.Pack.Utils
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Portable
--

module Data.Pack.Utils
  (
  -- * Information
    getPosition
  , getTotalSize
  , getRemaining
  , isFull
  --, sizeOfPacket
  ) where

import Data.ByteString (ByteString)
import Data.Pack.Packet
import Foreign.Ptr

-- | (Unpacker only) Get the position in the memory block.
getPosition :: Packet e Int
getPosition = mkInfoPacket $ \bs _ cur -> do
  top <- getTop bs
  return (cur, Right (cur `minusPtr` top))
{-# INLINE getPosition #-}

-- | (Unpacker only) Get the total size of the memory block.
getTotalSize :: Packet e Int
getTotalSize = mkInfoPacket $ \bs bottom cur -> do
  top <- getTop bs
  return (cur, Right (bottom `minusPtr` top))
{-# INLINE getTotalSize #-}

-- | (Unpacker only) Get a number of bytes to go in the memory block.
getRemaining :: Packet e Int
getRemaining = mkInfoPacket $ \_ bottom cur ->
  return (cur, Right (bottom `minusPtr` cur))
{-# INLINE getRemaining #-}

-- | (Unpacker only) Return True if source ByteString is fully consumed or target ByteString
-- is full.
isFull :: Packet e Bool
isFull = fmap (== 0) getRemaining
{-# INLINE isFull #-}

mkInfoPacket :: (ByteString -> Ptr () -> Ptr () -> IO (Ptr (), Either e a)) -> Packet e a
mkInfoPacket f = Packet
  (f, id, \_ _ p -> return p )
{-# INLINE mkInfoPacket #-}

-- /Packer only/ Get the position in the memory block.
-- withPosition :: (Int -> Packet e a) -> Packet e a
-- /Packer only/ Get the total size of the memory block.
-- withTotalSize :: (Int -> Packet e a) -> Packet e a
-- /Packer only/ Get a number of bytes to go in the memory block.
-- withRemaining :: (Int -> Packet e a) -> Packet e a
--res <- withFull $ \full -> do
--  full' <- isFull
--  dicase (if full' then ..) (if full then ..)
