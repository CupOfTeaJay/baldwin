-- Placeholder.

module Common where

import qualified Control.Monad.Random as MRand
import qualified System.Random as Rand
import qualified Data.Word as Word
import qualified Data.Int as Int
import qualified Linear

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

randVec :: Int
    -> Int
    -> Linear.Vector Float
randVec size seed =
    MRand.evalRand (MRand.replicateM size MRand.getRandom) (Rand.mkStdGen seed)

randMat :: Int
    -> Int
    -> Int
    -> Linear.Matrix Float
randMat rows cols seed =
    MRand.evalRand (MRand.replicateM rows $ MRand.replicateM cols MRand.getRandom) (Rand.mkStdGen seed)
