module Main where

import qualified Linear

main :: IO ()
main = do

    let matrixA = [[1, 3, 5],
                   [5, 7, 9],
                   [2, 4, 6]]

    let matrixB = [[2, 4, 6],
                   [8, 10, 12],
                   [3, 5, 7]]

    putStrLn "Adding to matrices..."
    print (Linear.addMat matrixA matrixB)

    putStrLn "Transposing a matrix..."
    print (Linear.transposeMat matrixB)
