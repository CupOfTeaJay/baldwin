module Main where

import qualified ActFuncs
import qualified Layer

main :: IO ()
main = do
    let exampleDenseLayer = Layer.init 3 2 ActFuncs.sigmoid :: Layer.DenseLayer
    putStrLn "Hello, Haskell!"
