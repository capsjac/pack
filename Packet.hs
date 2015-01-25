{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Packet (test)where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString, toForeignPtr)
import Foreign hiding (void)
import System.IO.Unsafe
import Debug.Trace


type Packer a = a -> Packet String a
newtype Packet e a = Packet
	{ unPacket ::
		( Ptr () -> Ptr () -> Ptr () -> IO (Ptr (), Either e a)
		, Int -> Int
		, Ptr () -> Ptr () -> Ptr () -> IO (Ptr ()) )
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

i8 :: Packer Int8
i8 a = Packet (needs 1 peek, (+1), pokeWith 1 poke a)
{-# INLINE i8 #-}

i16 :: Packer Int8
i16 = simple 2 peek poke
{-# INLINE i16 #-}

i32 :: Packer Int32
i32 = simple 4 peek poke
{-# INLINE i32 #-}

i64 :: Packer Int64
i64 = simple 8 peek poke
{-# INLINE i64 #-}

u8 :: Packer Word8
u8 = simple 1 peek poke
{-# INLINE u8 #-}

u16 :: Packer Word8
u16 = simple 2 peek poke
{-# INLINE u16 #-}

u32 :: Packer Word32
u32 = simple 4 peek poke
{-# INLINE u32 #-}

u64 :: Packer Word64
u64 = simple 8 peek poke
{-# INLINE u64 #-}

simple n get put = \a ->
	Packet (needs n get, (+n), pokeWith n put a)
{-# INLINE simple #-}

needs :: Int -> (Ptr a -> IO a) -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr (), Either String a)
needs n f _ bottom ptr | plusPtr ptr n <= bottom =
	(plusPtr ptr n,) . Right <$> f (castPtr ptr)
needs _ _ _ _ ptr =
	return (ptr, Left "not enough bytes.")
{-# INLINE needs #-}

pokeWith :: Int -> (Ptr a -> a -> IO ()) -> a -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
pokeWith n f a = \_ _ p -> f (castPtr p) a >> return (plusPtr p 1)
{-# INLINE pokeWith #-}

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
