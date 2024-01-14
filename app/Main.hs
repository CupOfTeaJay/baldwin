module Main where

import qualified Layer as L
import qualified ActFuncs as AF
import qualified LayerTypes as LT
import qualified Common

main :: IO ()
main = do
    -- This does not do anything yet. Conceptual structure of network so far.
    let inputLayer = L.initialize 784 16 AF.sigmoid 0 :: LT.DenseLayer
    let hidden0    = L.initialize 16 16  AF.sigmoid 1 :: LT.DenseLayer
    let hidden1    = L.initialize 16 16  AF.sigmoid 2 :: LT.DenseLayer
    let output     = L.initialize 16 10  AF.sigmoid 3 :: LT.DenseLayer

    putStrLn "Hello, Haskell!"
    print $ LT.weights output
