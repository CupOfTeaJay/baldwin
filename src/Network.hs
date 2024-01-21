{-
    Network.hs

    TODO: document.
-}

module Network where

import qualified Layer
import qualified LayerTypes
import qualified Linear

class Network network where
    forwardProp :: 
        Linear.Vector Float ->
        network ->
        network
    predict :: 
        Linear.Vector Float ->
        network ->
        Linear.Vector Float

-- Clever way to iterate over network? Current roadblock.
data FullyConnected = FullyConnected {layers :: [LayerTypes.DenseLayer]}
instance Network FullyConnected where
    forwardProp ingress network =
        FullyConnected {layers = map (Layer.activate ingress) $ layers network}
