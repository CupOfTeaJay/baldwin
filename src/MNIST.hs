module MNIST where

import Linear

parentDir :: String
parentDir = 
    "../_datasets/MNIST/"

trainImagesPath :: String
trainImagesPath =
    parentDir ++ "train-images-idx3-ubyte"

trainLabelsPath :: String
trainLabelsPath = 
    parentDir ++ "train-labels-idx1-ubyte"

testImagesPath :: String
testImagesPath = 
    parentDir ++ "t10k-images-idx3-ubyte"

testLabelsPath :: String
testLabelsPath =
    parentDir ++ "t10k-labels-idx1-ubyte"

loadTrainData :: IO (String, String)
loadTrainData = do
    images <- readFile trainImagesPath
    labels <- readFile trainLabelsPath
    return (images, labels)
