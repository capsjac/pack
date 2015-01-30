-- |
-- Module      : Data.Pack
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
-- Bidirectional packing module.
-- 
-- Usage:
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
-- Output:
-- 
-- > "d"
-- > "\166\176d"
-- > Right (-90,-80,100)
-- 
{-# LANGUAGE RankNTypes #-}

module Data.Pack
  ( -- * Main
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
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString)
import Data.Pack.Types
import Data.Pack.Primitives
import Data.Pack.ByteString
import Data.Pack.Structure
import Data.Pack.Space
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
  let Packet (get, size, _) = f (error "no touch")
  in unsafePerformIO $
    withForeignPtr (castForeignPtr fp) $ \origPtr -> do
      let ptr = origPtr `plusPtr` offset
      (p', v) <- get bs (plusPtr ptr (size 0)) ptr
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

