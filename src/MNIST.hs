{-
    MNIST.hs

    This module allows one to load the MNIST dataset located within 
    ../_datasets/MNIST

    The MNIST dataset is a collection of 70,000 handwritten digits (0-9) that 
    have been formatted into 28x28 pixel images. As of 09 JAN 2024, the raw 
    dataset can be downloaded from Kaggle at the following web address: 
    < https://www.kaggle.com/datasets/hojjatk/mnist-dataset >
-}

module MNIST (loadTrainData, 
              loadTestData, 
              displayData) where

import qualified Data.List.Split as SPL
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Linear

{-
    Header size for MNIST image data in bytes.
-}
imageHeaderSize :: Int
imageHeaderSize = 16

{-
    Width of MNIST images in pixels.
-}
imageWidth :: Int
imageWidth = 28

{-
    Header size for MNIST label data in bytes.
-}
labelHeaderSize :: Int
labelHeaderSize = 8

{-
    Total number of pixels in an MNIST image.
-}
pixelsPerImage :: Int
pixelsPerImage = imageWidth*imageWidth

{-
    Filename for MNIST test image data.
-}
testImagesFile :: String
testImagesFile = 
    "/t10k-images-idx3-ubyte"

{-
    Filename for MNIST training image data.
-}
trainImagesFile :: String
trainImagesFile =
    "/train-images-idx3-ubyte"

{-
    Filename for MNIST test label data.
-}
testLabelsFile :: String
testLabelsFile =
    "/t10k-labels-idx1-ubyte"

{-
    Filename for MNIST training label data.
-}
trainLabelsFile :: String
trainLabelsFile = 
    "/train-labels-idx1-ubyte"

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
loadTrainData :: String
    -> IO (Linear.Matrix Word8, Linear.Vector Word8)
loadTrainData filepath = do
    rawImages <- BS.readFile (filepath ++ trainImagesFile)
    rawLabels <- BS.readFile (filepath ++ trainLabelsFile)
    let hexImages = formatImageData(BS.unpack(BS.drop imageHeaderSize rawImages))
    let hexLabels = BS.unpack(BS.drop labelHeaderSize rawLabels)
    return (hexImages, hexLabels)

{-
    Reads the test data from baldwin/_datasets/MNIST and returns a tuple
    containing the test images and their corresponding labels.
-}
loadTestData :: String
    -> IO (Linear.Matrix Word8, Linear.Vector Word8)
loadTestData filepath = do
    rawImages <- BS.readFile (filepath ++ testImagesFile)
    rawLabels <- BS.readFile (filepath ++ testLabelsFile)
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
