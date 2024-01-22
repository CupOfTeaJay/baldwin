{-
    Network.hs

    TODO: document.
-}

module Network where

import qualified Layer
import qualified Linear

type Network = [Layer.Layer]

forwardProp ::
    Linear.Vector Double ->
    Network ->
    Linear.Vector Double
forwardProp ingress network =
    foldl Layer.activateLayer ingress network
