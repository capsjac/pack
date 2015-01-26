-- |
-- Module      : Data.Pack.Types
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
{-# LANGUAGE FlexibleInstances #-}

module Data.Pack.Types where

import Control.Applicative
import Data.ByteString
import Foreign

type Packer a = a -> Packet String a

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

-- | Derived from lens package
dimapP :: (b -> a) -> (a -> b) -> (a -> Packet e a) -> b -> Packet e b
dimapP ba ab f b = ab <$> f (ba b)

