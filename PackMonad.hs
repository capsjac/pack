module PackMonad (test, i8, pek, pac, Packet) where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString, toForeignPtr)
--import Data.Extensible
--import Data.Packer
import Foreign hiding (void)
import System.IO.Unsafe
import Debug.Trace
--newtype PackMonad a = PackMonad (() -> a) deriving Show

--instance Monad PackMonad where

--type Iso s t a b = forall p f. (Profunctor p, Functor f)
--	=> p a (f b) -> p s (f t)
--type Iso s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
--type Iso' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

--i8 :: (0 -> G Int8) -> (Int8 -> S ())

--type Packet a = a -> Packet' a
--(Resulting a' b' str') = do
--		a <- i8 a'
--		b <- i32 b'
--		val <- i64 (length str')
--		str <- chars (fromIntegral val) str'
--		return (Resulting a b str, )

--newtype Packet' a = Packet'
--	{ unPacket
--		:: Ptr Word8
--		-> Ptr Word8
--		-> Ptr Word8
--		-> (Ptr Word8, IO a, IO a) }

type Packer a = a -> Packet String a
newtype Packet e a = Packet
	{ unPacket
		:: Ptr Word8 -- ^ Top of ForeignPtr
		-> Ptr Word8 -- ^ Bottom of ForeignPtr
		-> Ptr Word8 -- ^ Pointer cursor
		-> (Ptr Word8, IO (Either e a), IO a)
		-- ^ (cursor + sizeof ByteString, peek, poke)
	}

instance Functor (Packet e) where
	fmap f m = Packet $ \top btm ptr ->
		let (ptr, get, put) = unPacket m top btm ptr
		in ( ptr
		   , get >>= return . fmap f
		   , put  >>= return . f )
	{-# INLINE fmap #-}

instance Applicative (Packet e) where
	pure a = Packet $ \_ _ ptr ->
		(ptr, return (Right a), return a)
	{-# INLINE pure #-}
	fm <*> m = do
		f <- fm
		a <- m
		return (f a)
	{-# INLINE (<*>) #-}

instance Monad (Packet e) where
	return = pure
	m >>= f = Packet $ \top btm ptr ->
		let (ptr', get, put) = unPacket m top btm ptr
		    f' x = unPacket (f x) top btm ptr'
		    (ptr'', _, _) = f' undefined
		in ( ptr''
		   , get >>= either (return.Left) (snd3 . f')
		   , put >>= trd3 . f'
		   )
		where snd3 (_, x, _) = x; trd3 (_, _, x) = x
	{-# INLINE (>>=) #-}

instance MonadIO (Packet e) where
	liftIO f = Packet $ \_ _ ptr -> (ptr, fmap Right f, f)
	{-# INLINE liftIO #-}

i8 :: Packer Int8
i8 v = Packet $ \top btm ptr ->
	( plusPtr ptr 1
	, if ptr < btm then Right <$> peek (castPtr ptr) else (return.Left) "too short"
	, poke (castPtr ptr) v >> return v)
{-# INLINE i8 #-}

pac :: (a -> Packet e a) -> a -> ByteString
pac f v =
	let Packet g = f v
	in unsafePerformIO $ do
		let (size, _, _) = g nullPtr nullPtr nullPtr
		fp <- B.mallocByteString (minusPtr size nullPtr)
		withForeignPtr fp $ \p -> do
			let (_, _, pok) = g p (plusPtr p (minusPtr size nullPtr)) p
			pok >> (return $! B.PS fp 0 (minusPtr size nullPtr))
{-# INLINE pac #-}
t x= trace (show x) x

pek :: (a -> Packet e a) -> ByteString -> Either e a
pek f z@(B.PS fp ptr sz) =
	let Packet g = f undefined
	in unsafePerformIO $ do
		withForeignPtr fp $ \p -> do
			let (_, pek, _) = g p (plusPtr p sz) p
			pek
{-# INLINE pek #-}

test = do
	putStrLn . show $ pac i8 100
	let ii88 ~(v,w,x) = do
		a <- i8 v
		b <- i8 w
		c <- i8 x
		return (a,b,c)
	putStrLn . show $ pac ii88 (-90,-80,100)
	putStrLn . show $ pek ii88 $ pac ii88 (-90,-80,100)



--type Object = Simple Iso Bool Int
---- p0 Int (f0 Int) -> p0 Bool (f0 Bool))
---- (Int -> f Int) -> (Bool -> f Bool)
--enm :: Object
--enm fctr = \bool -> fmap intToBool (fctr int)


--(a -> Packed) -> (Packed -> a)
--(\v->Pack(poke p v)) (\Unpackable p->peek p)

