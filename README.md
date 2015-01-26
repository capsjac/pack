pack
====

Bidirectional fast ByteString packer/unpacker for Haskell

    $ cabal install pack
    $ ghci
    ghci> import qualified Data.ByteString as BS
    ghci> import Data.Pack
    ghci> packing i8 10
    "\n"
    ghci> unpacking i8 (BS.pack "\n")
    Just 10

Stability: Experimental

Contribution: Welcome

Maintainer: capsjac

