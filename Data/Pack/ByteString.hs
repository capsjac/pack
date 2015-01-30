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
  , bytesWhile
  , bytesBefore
  , cstring
  , varchar
  --, signature
  , remainingBytes
  , remainingBytesCopy
  ) where

import Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Pack.Types
import Data.Pack.Primitives (i8)
import Data.Pack.Space (getRemaining)
import Data.Vector.Storable.Internal (getPtr)
import Foreign


-- | Slice a number of bytes from the source 'ByteString'.
-- The original block of memory is expected to live for the life of this
-- ByteString.
bytes :: Int -> Packer ByteString
bytes n = simpleBS $ \bs -> do
  let res = B.take n bs
  return (B.length res, Right res)
{-# INLINE bytes #-}

-- | Copy a number of bytes from the source 'ByteString'.
-- Similar to 'bytes' but this allow the original block of memory to go away.
bytesCopy :: Int -> Packer ByteString
bytesCopy n = fmap B.copy . bytes n
{-# INLINE bytesCopy #-}

-- | 'bytesWhile', applied to a predicate p, returns the longest prefix
-- (possibly empty) of bytes that satisfy p.
bytesWhile :: (Word8 -> Bool) -> Packer ByteString
bytesWhile pred = simpleBS $ \bs -> do
  let res = B.takeWhile pred bs
  return (B.length res, Right res)
{-# INLINE bytesWhile #-}

-- | 'bytesBefore' slices remaining ByteString at the first occurence of the
-- specified byte. It is more efficient than 'bytesWhile' as it is implemented
-- with memchr(3).
bytesBefore :: Word8 -> Packer ByteString
bytesBefore word = simpleBS $ \bs -> do
  let res = fst $ B.break (== word) bs
  return (B.length res, Right res)
{-# INLINE bytesBefore #-}

-- | Slice the remaining bytes.
remainingBytes :: Packer ByteString
remainingBytes = simpleBS $ \bs ->
  return (B.length bs, Right bs)
{-# INLINE remainingBytes #-}

-- | Similar to 'remainingBytes' but copy the remaining bytes.
remainingBytesCopy :: Packer ByteString
remainingBytesCopy = fmap B.copy . remainingBytes
{-# INLINE remainingBytesCopy #-}

-- | Variable-length NUL terminated string.
cstring :: Packer ByteString
cstring str = do
  bs <- bytesBefore 0 str
  i8 0
  return bs
{-# INLINE cstring #-}

-- | Fixed-length (possibly) NUL terminated string field.
-- Longer string will be trimmed and shorter one will be padded out with NUL.
varchar :: Int -> Packer ByteString
varchar upperLimit value = flip simpleBS (pp value) $ \bs -> do
  let field = B.take upperLimit bs
  let str = fst $ B.break (== 0) field
  return (B.length field, Right str)
  where
    pp v =
      let b = B.take upperLimit v
      in B.concat [b, B.replicate (upperLimit - B.length b) 0]
{-# INLINE varchar #-}

---- | Constant block of packet. Similar to 'unused' but specified ByteString
-- will be used to fill out rather than NUL bytes. Additionally, read content
-- is compared to the value and mismatch is reported.
--signature :: ByteString -> Packer (Maybe ByteString)
--signature upperLimit value = flip simpleBS (pp value) $ \bs -> do
--  let field = B.take upperLimit bs
--  let str = fst $ B.break (== 0) field
--  return (B.length field, Right str)
--  where
--    pp v =
--      let b = B.take upperLimit v
--      in B.concat [b, B.replicate (upperLimit - B.length b) 0]
--{-# INLINE signature #-}

simpleBS :: (ByteString -> IO (Int, Either String ByteString)) -> Packer ByteString
simpleBS get bs = asymmPacket get put size
  where
    size = B.length bs
    put cur = do
      top <- getTop bs
      B.memcpy (castPtr cur) top (fromIntegral size)
{-# INLINE simpleBS #-}

