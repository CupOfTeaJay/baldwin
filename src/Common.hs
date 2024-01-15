{-
    Common.hs

    TODO: document.
-}

module Common where

import qualified System.Random as Rand
import qualified Data.Word as Word
import qualified Data.Int as Int

type Uint8  = Word.Word8
type Uint16 = Word.Word16
type Uint32 = Word.Word32
type Uint64 = Word.Word64

type Sint8  = Int.Int8
type Sint16 = Int.Int16
type Sint32 = Int.Int32
type Sint64 = Int.Int64

getSeed :: IO Int
getSeed = do
    gen <- Rand.getStdGen
    let (seed, newGen) = Rand.random gen
    Rand.setStdGen newGen
    return seed