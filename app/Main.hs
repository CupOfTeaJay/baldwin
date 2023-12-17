module Main where

import qualified Linear

main :: IO ()
main = do

  let matrixA = [[1, 3, 5]]
  let matrixB = [[2, 4, 6]]

  putStrLn "Adding to matrices..."
  print (Linear.addMat matrixA matrixB)
