module MNIST where

import qualified Data.ByteString as BS
import qualified Data.List.Split as SPL
import Data.Word (Word8)
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

imageHeaderSize :: Int
imageHeaderSize = 16

labelHeaderSize :: Int
labelHeaderSize = 8

imageWidth :: Int
imageWidth = 28

pixelsPerImage :: Int
pixelsPerImage = imageWidth*imageWidth

formatImageData :: Linear.Vector Word8
    -> Linear.Matrix Word8
formatImageData vec =
    SPL.chunksOf pixelsPerImage vec

loadTrainData :: IO (Linear.Matrix Word8, Linear.Vector Word8)
loadTrainData = do
    rawImages <- BS.readFile trainImagesPath
    rawLabels <- BS.readFile trainLabelsPath
    let hexImages = formatImageData(BS.unpack(BS.drop imageHeaderSize rawImages))
    let hexLabels = BS.unpack(BS.drop labelHeaderSize rawLabels)
    return (hexImages, hexLabels)

loadTestData :: IO (Linear.Matrix Word8, Linear.Vector Word8)
loadTestData = do
    rawImages <- BS.readFile testImagesPath
    rawLabels <- BS.readFile testLabelsPath
    let hexImages = formatImageData(BS.unpack(BS.drop imageHeaderSize rawImages))
    let hexLabels = BS.unpack(BS.drop labelHeaderSize rawLabels)
    return (hexImages, hexLabels)
