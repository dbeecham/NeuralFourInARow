module Neural where

import Numeric.LinearAlgebra
import System.Random
import Utilities

type Network = [Matrix Double]
type TrainingSet = [(Matrix Double, Matrix Double)]


sigmoid :: Double -> Double
sigmoid x = 1/(1 + exp(-x))

sigmoid' :: Double -> Double
sigmoid' x = (sigmoid x) * (1 - (sigmoid x))


randomNetwork :: [Int] -> IO Network
randomNetwork sizes = do
    g <- getStdGen
    return (nfromList sizes (randoms g))


randomNetworks :: RandomGen g => g -> [Int] -> [Network]
randomNetworks g sizes =
    let length = sum $ zipWith (*) (drop 1 sizes) (map (+1) sizes)
        weights = partitionEvery length (randoms g)
    in map (nfromList sizes) weights


nfromLists :: [[[Double]]] -> Network
nfromLists xs = map fromLists xs

nfromList :: [Int] -> [Double] -> Network
nfromList sizes values = 
    let matrixSizes = map (\(x, y) -> (x + 1, y)) (pairs sizes)
        multTuple (x, y) = x*y
        partitions = partition (map multTuple matrixSizes) values
    in map2 (\(x, y) values -> (x><y) values) matrixSizes partitions

nflatten :: Network -> [Double]
nflatten = concat . concat . ntoLists

ntoLists :: Network -> [[[Double]]]
ntoLists network = map toLists network

fitness :: TrainingSet -> Network -> Double
fitness training network =
    let xs = map fst training
        ys = concat (map (toList . flatten . snd) training)
        actual = concat (map (toList . flatten . (forward network)) xs)
        differences = zipWith (-) ys actual
    in sqrt (sum (map (^2) differences))


forward :: Network -> Matrix Double -> Matrix Double
forward network input = foldl f input network
    where f last next = let last' = last ||| (((rows last)><1) (repeat 1))
            in cmap sigmoid (last' <> next)

