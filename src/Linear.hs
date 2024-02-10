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
type Vector a = [a]

{-
    Alias for a list of lists, effectively a matrix.
-}
type Matrix a = [Vector a]

{-
    Multiplies two matrices together.
-}
matMul ::
  (Num a) =>
  Matrix a ->
  Matrix a ->
  Matrix a
matMul matA matB =
  map (\row -> map (\col -> sum (zipWith (*) row col)) (matTrans matB)) matA

{-
    Multiplies a matrix by a scalar.
-}
matScale ::
  (Num a) =>
  a ->
  Matrix a ->
  Matrix a
matScale scalar mat =
  map (map (scalar *)) mat

{-
    Adds two matrices together.
-}
matSum ::
  (Num a) =>
  Matrix a ->
  Matrix a ->
  Matrix a
matSum matA matB =
  zipWith (zipWith (+)) matA matB

{-
    Transposes a matrix.
-}
matTrans ::
  (Num a) =>
  Matrix a ->
  Matrix a
matTrans mat =
  foldr (zipWith (:)) (repeat []) mat

{-
    Multiplies a matrix by a vector.
-}
matVecMul ::
  (Num a) =>
  Matrix a ->
  Vector a ->
  Vector a
matVecMul mat vec =
  map (\row -> sum (zipWith (*) row vec)) mat

{-
    TODO: document.
-}
randMat ::
  (Num a, Rand.Random a) =>
  Int ->
  Int ->
  Int ->
  Linear.Matrix a
randMat rows cols seed =
  MRand.evalRand (MRand.replicateM rows $ MRand.replicateM cols MRand.getRandom) (Rand.mkStdGen seed)

{-
    TODO: document.
-}
randVec ::
  (Num a, Rand.Random a) =>
  Int ->
  Int ->
  Linear.Vector a
randVec size seed =
  MRand.evalRand (MRand.replicateM size MRand.getRandom) (Rand.mkStdGen seed)

{-
    TODO: document.
-}
linearTransformMat ::
    (Num a) =>
    Matrix a ->
    a ->
    a ->
    Matrix a
linearTransformMat mat scale shift =
    map (map (\x -> scale * x + shift)) mat

{-
    Dot product of two vectors.
-}
vecDot ::
  (Num a) =>
  Vector a ->
  Vector a ->
  a
vecDot vecA vecB =
  sum (zipWith (*) vecA vecB)

{-
    Subtracts one vector from another.
-}
vecSub ::
  (Num a) =>
  Vector a ->
  Vector a ->
  Vector a
vecSub vecA vecB =
  zipWith (-) vecA vecB

{-
    Adds two vectors together.
-}
vecSum ::
  (Num a) =>
  Vector a ->
  Vector a ->
  Vector a
vecSum vecA vecB =
  zipWith (+) vecA vecB

