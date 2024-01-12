module Main where

import qualified ActFuncs
import qualified Layer

main :: IO ()
main = do
    -- This does not do anything yet. Conceptual structure of network so far.
    let inputLayer = Layer.init 784 16 ActFuncs.sigmoid :: Layer.DenseLayer
    let hidden0    = Layer.init 16 16  ActFuncs.sigmoid :: Layer.DenseLayer
    let hidden1    = Layer.init 16 16  ActFuncs.sigmoid :: Layer.DenseLayer
    let output     = Layer.init 16 10  ActFuncs.sigmoid :: Layer.DenseLayer
    putStrLn "Hello, Haskell!"
