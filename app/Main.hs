import Criterion.Main

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified VectorBuilder.Builder as VectorBuilder
import qualified VectorBuilder.Vector as VectorBuilder

--------------------------------------------------------------------------------

buildVector :: [a] -> Vector a
buildVector = Vector.fromList

buildVectorBuilder :: [a] -> Vector a
buildVectorBuilder = VectorBuilder.build . foldMap VectorBuilder.singleton

list :: Int -> [Int]
list n = replicate n 0

buildVectorBench n =
   bench ("vector " ++ show n) $ whnf buildVector (list n)

buildVectorBuilderBench n =
   bench ("vector-builder" ++ show n) $ whnf buildVectorBuilder (list n)

--------------------------------------------------------------------------------

concatVector :: [Vector a] -> Vector a
concatVector = Vector.concat

concatVectorBuilder :: [Vector a] -> Vector a
concatVectorBuilder = VectorBuilder.build . foldMap VectorBuilder.vector

listVector :: Int -> Int -> [Vector Int]
listVector n m = replicate n (Vector.replicate m 1)

concatVectorBench n =
   bench ("vector " ++ show n) $ whnf concatVector (listVector n n)

concatVectorBuilderBench n =
   bench ("vector-builder " ++ show n) $ whnf concatVectorBuilder (listVector n n)

--------------------------------------------------------------------------------

main = defaultMain [
  bgroup "build" $
      [ buildVectorBench , buildVectorBuilderBench ] <*> [1000, 2000 .. 10000],
  bgroup "concat" $
      [ concatVectorBench , concatVectorBuilderBench ] <*> [30, 60 .. 300]
  ]

