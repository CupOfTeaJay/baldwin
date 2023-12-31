module MNIST where

import qualified Data.ByteString as BS
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

loadTrainData :: IO (BS.ByteString, BS.ByteString)
loadTrainData = do
    images <- BS.readFile trainImagesPath
    labels <- BS.readFile trainLabelsPath
    return (images, labels)
