module Main where

import qualified Linear
import qualified MNIST
import Data.Word (Word8)

main :: IO ()
main = do
    loadedData <- MNIST.loadTrainData
    let (trainImages, trainLabels) = loadedData
    MNIST.displayData trainImages trainLabels 12
