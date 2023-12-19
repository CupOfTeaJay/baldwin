module Main where

main :: IO ()
main = do

let dense = Neural.DenseLayer {Neural.weights = Matrix.fromList 2 2 [1, 2, 3, 4], 
                               Neural.biases = Vector.fromList [1, 2]}

putStrLn("dense 'weights' matrix: " ++ show (Neural.weights dense))
