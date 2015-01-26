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
  , iptrsize
  , u16host
  , u32host
  , u64host
  , uptrsize
  , i16be
  , i32be
  , i64be
  , u16be
  , u32be
  , u64be
  , f32
  , f64
  , f32host
  , f64host
  , f32be
  , f64be
  , bytes
  , bytesCopy
  --, bytesWhile
  , remainingBytes
  , remainingBytesCopy
  , storable
  --, extensible
  --, vector
  --, vector'
  , pad
  , unused
  , alignedTo
  --, alignedWith
  --, isolate
  --, anchor
  , getPosition
  , getTotalSize
  , getRemaining
  --, cString
  --, cStringLen
  --, utf8
  --, utf8Len
  --, utf16
  --, utf16Len
  , enumOf
  --, bitfields
  --, dicase
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString as B (ByteString, copy)
import qualified Data.ByteString.Internal as B
import Data.Pack.Endianness
import Data.Pack.IEEE754
import Data.Pack.Types
import Foreign


-- | A 'Int8' 'Packet'.
i8 :: Packer Int8
i8 = simple 1 id id
{-# INLINE i8 #-}

-- | A 'Int16' 'Packet' serialized in little endian.
i16 :: Packer Int16
i16 = simple 2 (fromIntegral . le16Host) (le16Host . fromIntegral)
{-# INLINE i16 #-}

-- | A 'Int16' 'Packet' in the host endianness.
i16host :: Packer Int16
i16host = simple 2 id id
{-# INLINE i16host #-}

-- | A 'Int16' 'Packet' serialized in big endian.
i16be :: Packer Int16
i16be = simple 2 (fromIntegral . be16Host) (be16Host . fromIntegral)
{-# INLINE i16be #-}

-- | A 'Int32' 'Packet' serialized in little endian.
i32 :: Packer Int32
i32 = simple 4 (fromIntegral . le32Host) (le32Host . fromIntegral)
{-# INLINE i32 #-}

-- | A 'Int32' 'Packet' in the host endianness.
i32host :: Packer Int32
i32host = simple 4 id id
{-# INLINE i32host #-}

-- | A 'Int32' 'Packet' serialized in big endian.
i32be :: Packer Int32
i32be = simple 4 (fromIntegral . be32Host) (be32Host . fromIntegral)
{-# INLINE i32be #-}

-- | A 'Int64' 'Packet' serialized in little endian.
i64 :: Packer Int64
i64 = simple 8 (fromIntegral . le64Host) (le64Host . fromIntegral)
{-# INLINE i64 #-}

-- | A 'Int64' 'Packet' in the host endianness.
i64host :: Packer Int64
i64host = simple 8 id id
{-# INLINE i64host #-}

-- | A 'Int64' 'Packet' serialized in big endian.
i64be :: Packer Int64
i64be = simple 8 (fromIntegral . be64Host) (be64Host . fromIntegral)
{-# INLINE i64be #-}

-- | A host pointer-sized 'Int' 'Packet' in the host endianness.
iptrsize :: Packer Word
iptrsize a = simple (sizeOf a) id id a
{-# INLINE iptrsize #-}

-- | A 'Word8' 'Packet'.
u8 :: Packer Word8
u8 = simple 1 id id
{-# INLINE u8 #-}

-- | A 'Word16' 'Packet' serialized in little endian.
u16 :: Packer Word16
u16 = simple 2 le16Host le16Host
{-# INLINE u16 #-}

-- | A 'Word16' 'Packet' in the host endianness.
u16host :: Packer Word16
u16host = simple 2 id id
{-# INLINE u16host #-}

-- | A 'Word16' 'Packet' serialized in big endian.
u16be :: Packer Word16
u16be = simple 2 be16Host be16Host
{-# INLINE u16be #-}

-- | A 'Word32' 'Packet' serialized in little endian.
u32 :: Packer Word32
u32 = simple 4 le32Host le32Host
{-# INLINE u32 #-}

-- | A 'Word32' 'Packet' in the host endianness.
u32host :: Packer Word32
u32host = simple 4 id id
{-# INLINE u32host #-}

-- | A 'Word32' 'Packet' serialized in big endian.
u32be :: Packer Word32
u32be = simple 4 be32Host be32Host
{-# INLINE u32be #-}

-- | A 'Word64' 'Packet' serialized in little endian.
u64 :: Packer Word64
u64 = simple 8 le64Host le64Host
{-# INLINE u64 #-}

-- | A 'Word64' 'Packet' in the host endianness.
u64host :: Packer Word64
u64host = simple 8 id id
{-# INLINE u64host #-}

-- | A 'Word64' 'Packet' serialized in big endian.
u64be :: Packer Word64
u64be = simple 8 be64Host be64Host
{-# INLINE u64be #-}

-- | A host pointer-sized 'Word' 'Packet' in the host endianness.
uptrsize :: Packer Word
uptrsize a = simple (sizeOf a) id id a
{-# INLINE uptrsize #-}

-- | A IEEE754-'Float' 'Packet' serialized in little endian.
f32 :: Packer Float
f32 = simple 4 (wordToFloat.le32Host) (le32Host.floatToWord)
{-# INLINE f32 #-}

-- | A IEEE754-'Double' 'Packet' serialized in little endian.
f64 :: Packer Double
f64 = simple 8 (wordToDouble.le64Host) (le64Host.doubleToWord)
{-# INLINE f64 #-}

-- | A IEEE754-'Float' 'Packet' in the host endianness.
f32host :: Packer Float
f32host = simple 4 wordToFloat floatToWord
{-# INLINE f32host #-}

-- | A IEEE754-'Double' 'Packet' in the host endianness.
f64host :: Packer Double
f64host = simple 8 wordToDouble doubleToWord
{-# INLINE f64host #-}

-- | A IEEE754-'Float' 'Packet' serialized in big endian.
f32be :: Packer Float
f32be = simple 4 (wordToFloat.be32Host) (be32Host.floatToWord)
{-# INLINE f32be #-}

-- | A IEEE754-'Double' 'Packet' serialized in big endian.
f64be :: Packer Double
f64be = simple 8 (wordToDouble.be64Host) (be64Host.doubleToWord)
{-# INLINE f64be #-}

-- | A strict 'ByteString' 'Packet'.
bytes :: Int -> Packer ByteString
bytes n = simpleBS n id id
{-# INLINE bytes #-}

-- | A strict 'ByteString' 'Packet'.
bytesCopy :: Int -> Packer ByteString
bytesCopy n = fmap B.copy . bytes n
{-# INLINE bytesCopy #-}

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

-- | A 'Storable' 'Packet'.
storable :: Storable a => Packer a
storable a = simple (sizeOf a) id id a
{-# INLINE storable #-}

-- | Skip bytes, filling with specified byte.
pad :: Word8 -> Int -> Packet String ()
pad filler n = replicateM_ n (u8 filler) -- XXX make this faster
{-# INLINE pad #-}

-- | Skip bytes, filling with NUL bytes.
unused :: Int -> Packet String ()
unused = pad 0
{-# INLINE unused #-}

-- | 
alignedTo :: Int -> Packet String ()
alignedTo alignment = do
  pos <- getPosition
  unused (mod pos alignment)
{-# INLINE alignedTo #-}

--aligned :: Int -> Int -> Int
--aligned alignment n = (n + alignment - 1) .&. complement (alignment - 1)

simple :: Storable a => Int -> (a -> b) -> (b -> a) -> Packer b
simple = fixedPacket (const peek) poke
{-# INLINE simple #-}

fixedPacket :: (ByteString -> Ptr a -> IO a) -> (Ptr a -> a -> IO ()) -> Int -> (a -> b) -> (b -> a) -> Packer b
fixedPacket get put n toHost fromHost =
  dimapP fromHost toHost $ \a -> Packet
    ( \bs b p -> (plusPtr p n,) <$> checkBdr n b p (get bs (castPtr p))
    , (+n)
    , \_ _ p -> put (castPtr p) a >> return (plusPtr p n))
{-# INLINE fixedPacket #-}

checkBdr :: Int -> Ptr () -> Ptr () -> IO a -> IO (Either String a)
checkBdr n bottom ptr f | plusPtr ptr n <= bottom = Right <$> f
checkBdr _ _ _ _ = return (Left "not enough bytes.")
{-# INLINE checkBdr #-}

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

mkInfoPacket :: (ByteString -> Ptr () -> Ptr () -> IO (Ptr (), Either e a)) -> Packet e a
mkInfoPacket f = Packet
  (f, id, \_ _ p -> return p )
{-# INLINE mkInfoPacket #-}

getTop :: ByteString -> IO (Ptr a)
getTop (B.PS fp off _) = withForeignPtr fp $ \ptr ->
  return $ castPtr $ ptr `plusPtr` off
{-# INLINE getTop #-}

simpleBS :: Int -> (ByteString -> a) -> (a -> ByteString) -> Packer a
simpleBS n = fixedPacket get put n
  where
    get (B.PS fp _ _) cur = do
      offset <- withForeignPtr fp $ \orig ->
        return (cur `minusPtr` orig)
      return $ B.fromForeignPtr fp offset n
    put cur bs@(B.PS _ _ srclen) = do
      top <- getTop bs
      B.memcpy (castPtr cur) top (fromIntegral srclen)
{-# INLINE simpleBS #-}

--tag <- i32 (getTagId data)
--let getcase tag = case tag of
--  0 -> A <$> i32
--  1 -> B <$> f32
--let putcase dat = case dat of
--  A i -> i32 i
--  B f -> f32 f
--val <- dicase (getcase tag) (putcase data)
enumOf :: (Integral a, Enum b) => Packer a -> Packer b
enumOf = dimapP (fromIntegral.fromEnum) (toEnum.fromIntegral)

