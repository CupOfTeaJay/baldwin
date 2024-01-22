module Main where

import qualified ActFuncs
import qualified Linear
import qualified Layer
import qualified MNIST
import qualified Network

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

    -- Will this work???
    let inputLayer   = Layer.initDense 784 16 ActFuncs.sigmoid 0
    let hiddenLayer0 = Layer.initDense 16 16  ActFuncs.sigmoid 12
    let hiddenLayer1 = Layer.initDense 16 16  ActFuncs.sigmoid 39
    let outputLayer  = Layer.initDense 16 10  ActFuncs.sigmoid 9

    let myNetwork = [Layer.Dense inputLayer, 
                     Layer.Dense hiddenLayer0, 
                     Layer.Dense hiddenLayer1, 
                     Layer.Dense outputLayer]

    let result = Network.forwardProp (head testImages) myNetwork

    putStrLn $ show (Layer.weights hiddenLayer0)
    putStrLn $ show (Linear.translateMat (Layer.weights hiddenLayer0) (2) (-1))