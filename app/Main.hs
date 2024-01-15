module Main where

import qualified MNIST

main :: IO ()
main = do

    let pathToData = "_datasets/MNIST"

    putStrLn "Loading MNIST training data..."
    loadedTrainData <- MNIST.loadTrainData pathToData
    let (trainImages, trainLabels) = loadedTrainData

    putStrLn "Loading MNIST test data..."
    loadedTestData <- MNIST.loadTestData pathToData
    let (testImages, testLabels) = loadedTestData

    MNIST.displayData testImages testLabels 22
