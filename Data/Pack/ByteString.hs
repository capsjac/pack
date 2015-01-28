-- |
-- Module      : Data.Pack.ByteString
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Portable
--

module Data.Pack.ByteString
  (
  -- * Bytes
    bytes
  , bytesCopy
  --, bytesWhile
  --, bytesWhileChar
  , remainingBytes
  , remainingBytesCopy
  --, cstring
  --, varchar
  ) where

import Data.ByteString as B (ByteString, copy)
import qualified Data.ByteString.Internal as B
import Data.Pack.Types
import Data.Pack.Space (getRemaining)
import Data.Vector.Storable.Internal (getPtr)
import Foreign


-- | A strict 'ByteString' 'Packet'.
bytes :: Int -> Packer ByteString
bytes n = simpleBS n id id
{-# INLINE bytes #-}

-- | A strict 'ByteString' 'Packet'.
bytesCopy :: Int -> Packer ByteString
bytesCopy n = fmap B.copy . bytes n
{-# INLINE bytesCopy #-}

---- | A strict 'ByteString' 'Packet'.
--bytesWhile :: (Word8 -> Bool) -> Packer ByteString
--bytesWhile pred = limitedlength
--{-# INLINE bytesWhile #-}

---- | A strict 'ByteString' 'Packet'.
--bytesWhileChar :: Word8 -> Packer ByteString
--bytesWhileChar pred = B.memchr ptr 0 csize
--{-# INLINE bytesWhileChar #-}

-- | A strict 'ByteString' 'Packet'.
remainingBytes :: Packer ByteString
remainingBytes x = do
  n <- getRemaining
  simpleBS n id id x
{-# INLINE remainingBytes #-}

-- | A strict 'ByteString' 'Packet'.
remainingBytesCopy :: Packer ByteString
remainingBytesCopy = fmap B.copy . remainingBytes
{-# INLINE remainingBytesCopy #-}

---- | A strict 'ByteString' 'Packet'.
--cstring :: Packer ByteString
--cstring = bytesWhileWord8 0
--{-# INLINE cstring #-}

-- | A strict 'ByteString' 'Packet'.
--varchar :: Int -> Packer ByteString
--varchar upperLimit = simpleBS upperLimit id id
--{-# INLINE varchar #-}

simpleBS :: Int -> (ByteString -> a) -> (a -> ByteString) -> Packer a
simpleBS n = fixedPacket get put n
  where
    get (B.PS fp _ _) cur = do
      let offset = cur `minusPtr` getPtr fp
      return $ B.fromForeignPtr fp offset n
    put cur bs@(B.PS _ _ srclen) = do
      top <- getTop bs
      B.memcpy (castPtr cur) top (fromIntegral srclen)
{-# INLINE simpleBS #-}

