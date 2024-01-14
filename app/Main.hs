module Main where

import qualified Layer as L
import qualified ActFuncs as AF
import qualified LayerTypes as LT
import qualified Common

main :: IO ()
main = do

    -- Get seeds.
    s1 <- Common.getSeed
    s2 <- Common.getSeed
    s3 <- Common.getSeed
    s4 <- Common.getSeed

    -- This does not do anything yet. Conceptual structure of network so far.
    let inputLayer = L.initialize 784 16 AF.sigmoid s1 :: LT.DenseLayer
    let hidden0    = L.initialize 16 16  AF.sigmoid s2 :: LT.DenseLayer
    let hidden1    = L.initialize 16 16  AF.sigmoid s3 :: LT.DenseLayer
    let output     = L.initialize 16 10  AF.sigmoid s4 :: LT.DenseLayer

    -- Print for fun to see where we're at.
    putStrLn "Hello, Haskell!"
    print $ LT.weights hidden0
    print $ LT.weights hidden1
