-- |
-- Module      : Data.Pack
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : GHC, Unknown
--
-- The pack package provides bidirectional packing and unpacking (aka.
-- (de)serialise) Haskell values to and from strict 'ByteString's.
-- Both operations are faster than binary and cereal package so that it can be
-- used in performance sensible operations. Bytestring allocation is batched and
-- done before packing any values to avoid performance loss. The pack package
-- does not provide specific typeclasses (it's a good idea, though) to clear out
-- ambiguity of serialisation format.
-- 
-- * Migration Sheet from binary, cereal and packer
-- 
-- [@encode, runPacking@] 'packing' (packer).
-- 
-- [@decode, runUnpacking@] 'unpacking' (packer).
-- 
-- [@getWord8, putWord8@] 'i8' (put value) and 'u8' (put value).
-- 
-- [@getWord*, putWord*@] {i,u}{8,16,32,64}{,b,host} (put value).
-- 
-- [@skip@] 'unused', or 'pad'.
-- 
-- [@isEmpty, bytesRead@] 'isFull' and 'getPosition'.
-- 
-- [@getByteString@] 'bytes'.
-- 
-- [@getLazyByteStringNul@] 'cstring'.
-- 
-- [@getWordhost@] 'uptrsize' and 'iptrsize'.
-- 
-- * Example
-- 
-- > pactest = do
-- >   putStrLn . show $ packing i8 100
-- >   let i8i8i8 ~(v,w,x) = do -- `~` is important!
-- >         a <- i8 v
-- >         b <- i8 w
-- >         c <- i8 x
-- >         return (a,b,c)
-- >       {-# INLINE i8i8i8 #-} -- For efficiency
-- >   putStrLn . show $ packing i8i8i8 (-90,-80,100)
-- >   putStrLn . show $ unpacking i8i8i8 $ packing i8i8i8 (-90,-80,100)
-- 
-- * Output
-- 
-- > "d"
-- > "\166\176d"
-- > Right (-90,-80,100)
-- 
{-# LANGUAGE RankNTypes #-}

module Data.Pack
  (
  -- * Main
    packing
  , unpacking
  , packet
  , pactest
  , Packer
  , Packet
  , module Data.Pack.Primitives
  , module Data.Pack.ByteString
  , module Data.Pack.Structure
  , module Data.Pack.Space
  , module Data.Pack.Utils
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString)
import Data.Pack.Packet
import Data.Pack.Primitives
import Data.Pack.ByteString
import Data.Pack.Structure
import Data.Pack.Space
import Data.Pack.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe


-- | Pack with a monadic action ('Packer' a) and return the 'ByteString'. 
packing :: (a -> Packet e a) -> a -> ByteString
packing f v =
  let Packet (_, size, put) = f v
  in unsafePerformIO $ do
    let len = size 0
    fp <- B.mallocByteString len
    let bs = B.PS fp 0 len
    withForeignPtr fp $ \ptr -> do
      endPtr <- put bs (plusPtr ptr len) (castPtr ptr)
      return bs
{-# INLINE packing #-}

-- | Unpack a 'ByteString' using a 'Packer'.
unpacking :: (a -> Packet e a) -> ByteString -> Either e a
unpacking f bs@(B.PS fp offset len) =
  let Packet (get, _, _) = f (error "unpacker cannot touch argument")
  in unsafePerformIO $
    withForeignPtr (castForeignPtr fp) $ \origPtr -> do
      let ptr = origPtr `plusPtr` offset
      (p', v) <- get bs (plusPtr ptr len) ptr
      return v
{-# INLINE unpacking #-}

-- | Prism from lens package.
packet :: Packer a -> Prism' ByteString a
packet packer = prism' (packing packer)
  (either (const Nothing) Just . unpacking packer)

-- | Readme.
pactest :: IO ()
pactest = do
  putStrLn . show $ packing i8 100
  let i8i8i8 ~(v,w,x) = do
        a <- i8 v
        b <- i8 w
        c <- i8 x
        return (a,b,c)
      {-# INLINE i8i8i8 #-}
  putStrLn . show $ packing i8i8i8 (-90,-80,100)
  putStrLn . show $ unpacking i8i8i8 $ packing i8i8i8 (-90,-80,100)

