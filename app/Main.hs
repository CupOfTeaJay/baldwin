{-
    TODO:
    - Implement backpropogation.
    - Refactor library to use arbitrarily sized "tensors" instead of vectors and matrices.
    - Refactor MNIST so it and future datasets can be loaded by some generic dataLoader.
    - Do we need linearTranslateMat? Maybe randMat can somehow gen (-1, 1).
-}
module Main where

import qualified ActFuncs
import qualified Common
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

    seed0 <- Common.getSeed
    seed1 <- Common.getSeed
    seed2 <- Common.getSeed
    seed3 <- Common.getSeed

    let inputLayer   = Layer.initDense 784 16 ActFuncs.sigmoid seed0
    let hiddenLayer0 = Layer.initDense 16 16  ActFuncs.sigmoid seed1
    let hiddenLayer1 = Layer.initDense 16 16  ActFuncs.sigmoid seed2
    let outputLayer  = Layer.initDense 16 10  ActFuncs.sigmoid seed3

    let myNetwork = [Layer.Dense inputLayer, 
                     Layer.Dense hiddenLayer0, 
                     Layer.Dense hiddenLayer1, 
                     Layer.Dense outputLayer]

    let result = Network.forwardProp (head testImages) myNetwork

    putStrLn $ show (result)

