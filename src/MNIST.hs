module MNIST where

import Linear

parentDir :: String
parentDir = 
    "../_datasets/MNIST/"

trainImages :: String
trainImages =
    parentDir ++ "train-images-idx3-ubyte"

trainLabels :: String
trainLabels = 
    parentDir ++ "train-labels-idx1-ubyte"

testImages :: String
testImages = 
    parentDir ++ "t10k-images-idx3-ubyte"

testLabels :: String
testLabels =
    parentDir ++ "t10k-labels-idx1-ubyte"

loadTrainData :: IO (String, String)
loadTrainData = do
    images <- readFile trainImages
    labels <- readFile trainLabels
    return (images, labels)
