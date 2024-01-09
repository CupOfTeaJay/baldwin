{-
    MNIST.hs

    This module allows one to load the MNIST dataset located within 
    ../_datasets/MNIST

    The MNIST dataset is a collection of 70,000 handwritten digits (0-9) that 
    have been formatted into 28x28 pixel images. As of 09 JAN 2024, the raw 
    dataset can be downloaded from Kaggle at the following web address: 
    < https://www.kaggle.com/datasets/hojjatk/mnist-dataset >
-}

module MNIST where

import qualified Data.ByteString as BS
import qualified Data.List.Split as SPL
import Data.Word (Word8)
import Linear

{-
    Parent directory of the MNIST dataset.
-}
parentDir :: String
parentDir = 
    "_datasets/MNIST/"

{-
    Filepath for MNIST training image data.
-}
trainImagesPath :: String
trainImagesPath =
    parentDir ++ "train-images-idx3-ubyte"

{-
    Filepath for MNIST training label data.
-}
trainLabelsPath :: String
trainLabelsPath = 
    parentDir ++ "train-labels-idx1-ubyte"

{-
    Filepath for MNIST test image data.
-}
testImagesPath :: String
testImagesPath = 
    parentDir ++ "t10k-images-idx3-ubyte"

{-
    Filepath for MNIST test label data.
-}
testLabelsPath :: String
testLabelsPath =
    parentDir ++ "t10k-labels-idx1-ubyte"

{-
    Header size for MNIST image data in bytes.
-}
imageHeaderSize :: Int
imageHeaderSize = 16

{-
    Header size for MNIST label data in bytes.
-}
labelHeaderSize :: Int
labelHeaderSize = 8

{-
    Width of MNIST images in pixels.
-}
imageWidth :: Int
imageWidth = 28

{-
    Total number of pixels in an MNIST image.
-}
pixelsPerImage :: Int
pixelsPerImage = imageWidth*imageWidth

{-
    Splits the raw image data (a list of bytes) into a list of lists. Each
    element of the resulting data structure is a list of 28x28 = 784 bytes.
    These are our images.
-}
formatImageData :: Linear.Vector Word8
    -> Linear.Matrix Word8
formatImageData vec =
    SPL.chunksOf pixelsPerImage vec

{-
    Reads the training data from baldwin/_datasets/MNIST and returns a tuple
    containing the training images and their corresponding labels.
-}
loadTrainData :: IO (Linear.Matrix Word8, Linear.Vector Word8)
loadTrainData = do
    rawImages <- BS.readFile trainImagesPath
    rawLabels <- BS.readFile trainLabelsPath
    let hexImages = formatImageData(BS.unpack(BS.drop imageHeaderSize rawImages))
    let hexLabels = BS.unpack(BS.drop labelHeaderSize rawLabels)
    return (hexImages, hexLabels)

{-
    Reads the test data from baldwin/_datasets/MNIST and returns a tuple
    containing the test images and their corresponding labels.
-}
loadTestData :: IO (Linear.Matrix Word8, Linear.Vector Word8)
loadTestData = do
    rawImages <- BS.readFile testImagesPath
    rawLabels <- BS.readFile testLabelsPath
    let hexImages = formatImageData(BS.unpack(BS.drop imageHeaderSize rawImages))
    let hexLabels = BS.unpack(BS.drop labelHeaderSize rawLabels)
    return (hexImages, hexLabels)

{-
    Converts a pixel value (0-255) to a character. This can be used to crudely
    display the images to confirm the data has been loaded correctly.
-}
pixelToChar :: Word8 -> Char
pixelToChar pixel
    | pixel == 0 = ' '
    | otherwise  = '*'

{-
    Roughly displays an MNIST image with its corresponding label.
-}
displayData :: Linear.Matrix Word8
    -> Linear.Vector Word8
    -> Int
    -> IO ()
displayData imageData labelData index = do
    putStrLn $ "Label: " ++ show (labelData!!index)
    mapM_ putStrLn $ SPL.chunksOf imageWidth (map pixelToChar (imageData!!index))
