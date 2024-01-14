-- Placeholder.

module Common where

import qualified Control.Monad.Random as MRand
import qualified System.Random as Rand
import qualified Linear

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
