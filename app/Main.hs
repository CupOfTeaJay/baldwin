module Main where

import qualified Linear

main :: IO ()
main = do

    let matA = [[1,2,3],
                [4,5,6],
                [7,8,9]]

    let matB = [[0,2,1],
                [3,4,6],
                [7,9,8]]

    let vec = [2, 2, 2]

    putStrLn("Multipying two matrices:" ++ show(Linear.matMul matA matB))

    putStrLn("Multipying a matrix and a vector:" ++ show(Linear.matVecMul matA vec))
