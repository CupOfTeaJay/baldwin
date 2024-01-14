module Main where

import qualified Layer as L
import qualified ActFuncs as AF
import qualified LayerTypes as LT
import qualified Common

main :: IO ()
main = do

    -- Get seeds.
    s1 <- Common.getSeed

    -- This does not do anything yet. Conceptual structure of network so far.
    let inputLayer = L.initialize 3 2 AF.sigmoid s1 :: LT.DenseLayer

    let nextActivation = L.forwardProp inputLayer [1, 2, 3]

    print $ LT.weights inputLayer
    print nextActivation
