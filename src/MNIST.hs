module MNIST where

import Linear

loadTrainingData :: String -> IO String
loadTrainingData filepath =
    readFile filepath
