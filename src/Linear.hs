{-
    Linear.hs

    This module contains types and functions for performing linear algebra
    operations.
-}

module Linear where

import qualified Control.Monad.Random as MRand
import qualified System.Random as Rand

{-
    Alias for a list, effectively a vector.
-}
type Vector dataType = [dataType]

{-
    Alias for a list of lists, effectively a matrix.
-}
type Matrix dataType = [Vector dataType]

{-
    Multiplies two matrices together.
-}
matMul ::
  (Num dataType) =>
  Matrix dataType ->
  Matrix dataType ->
  Matrix dataType
matMul matA matB =
  map (\row -> map (\col -> sum (zipWith (*) row col)) (matTrans matB)) matA

{-
    Multiplies a matrix by a scalar.
-}
matScale ::
  (Num dataType) =>
  dataType ->
  Matrix dataType ->
  Matrix dataType
matScale scalar mat =
  map (map (scalar *)) mat

{-
    Adds two matrices together.
-}
matSum ::
  (Num dataType) =>
  Matrix dataType ->
  Matrix dataType ->
  Matrix dataType
matSum matA matB =
  zipWith (zipWith (+)) matA matB

{-
    Transposes a matrix.
-}
matTrans ::
  (Num dataType) =>
  Matrix dataType ->
  Matrix dataType
matTrans mat =
  foldr (zipWith (:)) (repeat []) mat

{-
    Multiplies a matrix by a vector.
-}
matVecMul ::
  (Num dataType) =>
  Matrix dataType ->
  Vector dataType ->
  Vector dataType
matVecMul mat vec =
  map (\row -> sum (zipWith (*) row vec)) mat

{-
    TODO: document.
-}
randMat ::
  (Num dataType, Rand.Random dataType) =>
  Int ->
  Int ->
  Int ->
  Linear.Matrix dataType
randMat rows cols seed =
  MRand.evalRand (MRand.replicateM rows $ MRand.replicateM cols MRand.getRandom) (Rand.mkStdGen seed)

{-
    TODO: document.
-}
randVec ::
  (Num dataType, Rand.Random dataType) =>
  Int ->
  Int ->
  Linear.Vector dataType
randVec size seed =
  MRand.evalRand (MRand.replicateM size MRand.getRandom) (Rand.mkStdGen seed)

{-
    Dot product of two vectors.
-}
vecDot ::
  (Num dataType) =>
  Vector dataType ->
  Vector dataType ->
  dataType
vecDot vecA vecB =
  sum (zipWith (*) vecA vecB)

{-
    Subtracts one vector from another.
-}
vecSub ::
  (Num dataType) =>
  Vector dataType ->
  Vector dataType ->
  Vector dataType
vecSub vecA vecB =
  zipWith (-) vecA vecB

{-
    Adds two vectors together.
-}
vecSum ::
  (Num dataType) =>
  Vector dataType ->
  Vector dataType ->
  Vector dataType
vecSum vecA vecB =
  zipWith (+) vecA vecB
