-- |
-- Module      : Data.Pack
-- License     : BSD-style
-- Maintainer  : capsjac <capsjac at gmail dot com>
-- Stability   : Experimental
-- Portability : Unknown
--
-- Usage:
-- 
-- > pactest = do
-- >   putStrLn . show $ packing i8 100
-- >   let i8i8i8 ~(v,w,x) = do -- `~` is important!
-- >     a <- i8 v
-- >     b <- i8 w
-- >     c <- i8 x
-- >     return (a,b,c)
-- >   {-# INLINE i8i8i8 #-} -- For efficiency
-- >   putStrLn . show $ packing i8i8i8 (-90,-80,100)
-- >   putStrLn . show $ unpacking i8i8i8 $ packing i8i8i8 (-90,-80,100)
-- 
-- Output:
-- 
-- > "d"
-- > "\166\176d"
-- > Right (-90,-80,100)

{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Data.Pack
  ( packing
  , unpacking
  , packet
  , pactest
  , module Data.Pack.Primitives
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B (ByteString(..), mallocByteString, toForeignPtr)
import Data.Pack.Primitives
import Foreign
import System.IO.Unsafe


packing :: (a -> Packet e a) -> a -> ByteString
packing f v =
  let Packet (_, size, put) = f v
  in unsafePerformIO $ do
    fp <- B.mallocByteString (size 0)
    withForeignPtr fp $ \ptr -> do
      endPtr <- put fp (plusPtr ptr (size 0)) (castPtr ptr)
      return $! B.PS fp 0 (minusPtr endPtr ptr)
{-# INLINE packing #-}

unpacking :: (a -> Packet e a) -> ByteString -> Either e a
unpacking f z@(B.PS fp ptr len) =
  let Packet (get, size, _) = f (error "no touch")
  in unsafePerformIO $
    withForeignPtr (castForeignPtr fp) $ \ptr -> do
      (p', v) <- get fp (plusPtr ptr (size 0)) ptr
      return v
{-# INLINE unpacking #-}

-- | Prism.
packet :: Packer a -> Prism' ByteString a
packet packer = prism' (packing packer)
  (either (const Nothing) Just . unpacking packer)

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

