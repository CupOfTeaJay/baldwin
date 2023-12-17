module Linear where

type Matrix dataType = [[dataType]]

addMat :: Num dataType =>
    Matrix dataType ->
    Matrix dataType ->
    Matrix dataType
addMat matA matB = 
    zipWith(zipWith (+)) matA matB

multMat :: Num dataType =>
    Matrix dataType ->
    Matrix dataType ->
    Matrix dataType
multMat matA matB = 
    map(\row -> map(\col -> sum(zipWith(*) row col)) (transposeMat matB)) matA

scaleMat :: Num dataType =>
           dataType ->
    Matrix dataType ->
    Matrix dataType
scaleMat scalar mat = 
    map(map(scalar*)) mat
    
transposeMat :: Num dataType =>
    Matrix dataType ->
    Matrix dataType
transposeMat mat = 
    foldr(zipWith(:)) (repeat []) mat
