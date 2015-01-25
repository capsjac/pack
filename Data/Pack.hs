-- |
-- Module      : Data.Pack
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
{-# LANGUAGE FlexibleInstances #-}

module Data.Pack
  ( pac
  , pek
  , test
  , module Data.Pack.Primitives
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString, toForeignPtr)
import Data.Pack.Primitives
import Foreign
import System.IO.Unsafe
import Debug.Trace


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

pac :: Packer a -> a -> ByteString
pac f v =
	let Packet (_, size, put) = f v
	in unsafePerformIO $ do
		fp <- B.mallocByteString (size 0)
		withForeignPtr fp $ \ptr -> do
			endPtr <- put ptr (plusPtr ptr (size 0)) ptr
			return $! B.PS (castForeignPtr fp) 0 (minusPtr endPtr ptr)
{-# INLINE pac #-}

pek :: (a -> Packet e a) -> ByteString -> Either e a
pek f z@(B.PS fp ptr len) =
	let Packet (get, size, _) = f (error "no touch")
	in unsafePerformIO $
		withForeignPtr (castForeignPtr fp) $ \ptr -> do
			(p', v) <- get ptr (plusPtr ptr (size 0)) ptr
			return v
{-# INLINE pek #-}

test = do
	putStrLn . show $ pac i8 100
	let ii88 ~(v,w,x) = do
		a <- i8 v
		b <- i8 w
		c <- i8 x
		return (a,b,c)
	    {-# INLINE ii88 #-}
	putStrLn . show $ pac ii88 (-90,-80,100)
	putStrLn . show $ pek ii88 $ pac ii88 (-90,-80,100)

