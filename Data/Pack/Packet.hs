-- |
-- Module      : Data.Pack.Packet
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Portable
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Data.Pack.Packet where

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString
import Data.ByteString.Internal (ByteString(..))
import Data.Vector.Storable.Internal (getPtr)
import Foreign

-- | A 'Packer' recieves one value to pack and returns one 'Packet' which is
-- used to unpack the value of same type.
type Packer a = a -> Packet String a

-- | Bidirectional packing/unpacking Monad.
newtype Packet e a = Packet
  { unPacket ::
    ( ByteString -> Ptr () -> Ptr () -> IO (Ptr (), Either e a)
    , Int -> Int
    , ByteString -> Ptr () -> Ptr () -> IO (Ptr ()) )
  }

instance Functor (Packet e) where
  fmap f m =
    let (get, size, put) = unPacket m
        fmapget (p, e) = return (p, fmap f e)
    in Packet (\t b p -> get t b p >>= fmapget, size, put)
  {-# INLINE fmap #-}

instance Applicative (Packet e) where
  pure a = Packet
    ( \_ _ p -> return (p, Right a)
    , id
    , \_ _ p -> return p)
  {-# INLINE pure #-}
  Packet (fg, fs, fp) <*> Packet (get, size, put) = Packet
    ( \t b p ->
      fg t b p >>= \(p', ef) ->
      either (\l -> return (p', Left l)) (\f ->
        get t b p' >>= \(p'', ev) ->
        either (\l -> return (p'', Left l)) (\v ->
          return (p'', Right $ f v)
          ) ev
        ) ef
    , size . fs
    , \t b p -> fp t b p >>= put t b
    )
  {-# INLINE (<*>) #-}

instance Monad (Packet e) where
  return = pure
  {-# INLINE return #-}
  Packet (mg, ms, mp) >>= f =
    let Packet (_, size, set) = f undefined
    in Packet
    ( \t b p ->
      mg t b p >>= \(p', eg) ->
      either (\r -> return (p', Left r)) (\v ->
        let Packet (get, _, _) = f v
        in get t b p'
        ) eg
    , size . ms
    , \t b p -> mp t b p >>= set t b
    )
  {-# INLINE (>>=) #-}

-- instance Alternative (Packet e) where

-- | Derived from lens package. Bidirectional mapping.
dimapP :: (b -> a) -> (a -> b) -> (a -> Packet e a) -> b -> Packet e b
dimapP ba ab f b = ab <$> f (ba b)
{-# INLINE dimapP #-}

-- | 'fixedPacket' for Storable types.
simple :: Storable a => Int -> (a -> b) -> (b -> a) -> Packer b
simple = fixedPacket (const peek) poke
{-# INLINE simple #-}

-- | Generate a fixed-length 'Packer'.
fixedPacket :: (ByteString -> Ptr a -> IO a) -> (Ptr a -> a -> IO ()) -> Int -> (a -> b) -> (b -> a) -> Packer b
fixedPacket get put n toHost fromHost =
  dimapP fromHost toHost $ \a -> Packet
    ( \bs b p -> (plusPtr p n,) <$> checkBdr n b p (get bs (castPtr p))
    , (+n)
    , \_ _ p -> put (castPtr p) a >> return (plusPtr p n))
{-# INLINE fixedPacket #-}

-- | Generate a variable-length 'Packer'.
asymmPacket :: (ByteString -> IO (Int, Either String a)) -> (Ptr a -> IO ()) -> Int -> Packet String a
asymmPacket get put putsize = Packet
    ( \(PS fp _ _) bottom cur -> do 
      let offset = cur `minusPtr` getPtr fp
      let bs = PS fp offset (bottom `minusPtr` cur) -- (len - (offset - off))
      (getsize, value) <- get bs
      -- bound check is delegated to ByteString.*
      --when (getsize < 0) $ error "Data.Pack: negative length"
      return (plusPtr cur getsize, value)
    
    , (+ putsize)
    , \_ _ p -> put (castPtr p) >> return (plusPtr p putsize))
{-# INLINE asymmPacket #-}

-- | Unpackers should not read out of memory, so check the border here.
checkBdr :: Int -> Ptr () -> Ptr () -> IO a -> IO (Either String a)
checkBdr n bottom ptr f | plusPtr ptr n <= bottom = Right <$> f
checkBdr _ _ _ _ = return (Left "not enough bytes.")
{-# INLINE checkBdr #-}

-- | Get a pointer to the head of given 'ByteString'.
getTop :: ByteString -> IO (Ptr a)
getTop (PS fp off _) =
  return $ castPtr $ getPtr fp `plusPtr` off
{-# INLINE getTop #-}

