-- Placeholder text.

module Network where

import qualified LayerTypes
import qualified Linear
import qualified Layer

class Network network where
    forwardProp :: Linear.Vector Float
        -> network
        -> network
    predict :: Linear.Vector Float
        -> network
        -> Linear.Vector Float

data FullyConnected = FullyConnected {layers :: [LayerTypes.DenseLayer]}
instance Network FullyConnected where
    forwardProp ingress network =
        FullyConnected {layers = map (Layer.activate ingress) $ layers network}
