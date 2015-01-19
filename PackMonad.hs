module PackMonad (test, i8, pek, pac, Packet) where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString, toForeignPtr)
--import Data.Extensible
--import Data.Packer
import Foreign
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

type Packet a = a -> Packet' a
newtype Packet' a = Packet'
	{ unPacket
		:: Ptr Word8
		-> Ptr Word8
		-> Ptr Word8
		-> (Ptr Word8, IO a, IO a) }

instance Functor Packet' where
	fmap f m = Packet' $ \top btm ptr ->
		let (pos, pek, pok) = unPacket m top btm ptr
		in (pos, pek >>= return . f, pok >>= return . f)

instance Applicative Packet' where
	pure a = Packet' $ \_ _ ptr -> (ptr, return a, return a)
	fm <*> m = fm >>= \f -> m >>= \v -> return (f v)

instance Monad Packet' where
	return = pure
	f >>= m = Packet' $ \top btm ptr ->
		let (pos, pek, pok) = unPacket f top btm ptr
		in ( fst3 $ unPacket (m undefined) top btm pos
			, pek >>= \v -> snd3 $ unPacket (m v) top btm pos
			, pok >>= \v -> trd3 $ unPacket (m v) top btm pos
			)
		where fst3 (x, _, _) = x; snd3 (_, x, _) = x; trd3 (_, _, x) = x

instance MonadIO Packet' where
    liftIO f = Packet' $ \_ _ ptr -> (ptr, f, f)

i8 :: Packet Int8
i8 v = Packet' $ \top btm ptr ->
	--if not (ptr < btm) then error "size"
	( plusPtr ptr 1
	, peek (castPtr ptr)
	, poke (castPtr ptr) v >> return v )

pac :: Int -> Packet a -> a -> ByteString
pac sz f v =
	let Packet' g = f v
	in unsafePerformIO $ do
		fp <- B.mallocByteString sz
		withForeignPtr fp $ \p -> do
			let (ptr, pek, pok) = g p (plusPtr p sz) p
			pok >> (return $! B.PS fp 0 (minusPtr ptr p))
t x= trace (show x) x
pek :: Packet a -> ByteString -> a
pek f (B.PS fp ptr sz) =
	let Packet' g = f (error "no touch")
	in unsafePerformIO $ do
		withForeignPtr fp $ \p -> do
			let (ptr, pek, pok) = g p (plusPtr p sz) p
			pek

test = pac 1 i8 10


--type Object = Simple Iso Bool Int
---- p0 Int (f0 Int) -> p0 Bool (f0 Bool))
---- (Int -> f Int) -> (Bool -> f Bool)
--enm :: Object
--enm fctr = \bool -> fmap intToBool (fctr int)


--(a -> Packed) -> (Packed -> a)
--(\v->Pack(poke p v)) (\Unpackable p->peek p)

