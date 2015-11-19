> module Neural where


Neural Networks
===============


This module aims to implement creation and evaluation of basic neural
networks, along with a way to optimize the network (through backpropagation)
to fit a training set.

> import Numeric.LinearAlgebra
> import System.Random
> import Utilities


A network is a list of weight-matrices. Each entry in the list is a single layer
in the network, each column of a matrix defines the weighs of a neuron.

> type Network = [Matrix Double]

To train a network, it's useful to define a training set (x, f(x)) which
the network aims to fit.

> type TrainingSet = [(Matrix Double, Matrix Double)]



Neuron activation
-----------------

The sigmoid function is a good activation function because it has such an
easy to compute derivative, which is useful in backpropagation.

> sigmoid :: Double -> Double
> sigmoid x = 1/(1 + exp(-x))

> sigmoid' :: Double -> Double
> sigmoid' x = (sigmoid x) * (1 - (sigmoid x))



Creating networks
-----------------

> nfromLists :: [[[Double]]] -> Network
> nfromLists xs = map fromLists xs

> nfromList :: [Int] -> [Double] -> Network
> nfromList sizes values = 
>     let matrixSizes = map (\(x, y) -> (x + 1, y)) (pairs sizes)
>         multTuple (x, y) = x*y
>         partitions = partition (map multTuple matrixSizes) values
>     in map2 (\(x, y) values -> (x><y) values) matrixSizes partitions

> randomNetwork :: [Int] -> IO Network
> randomNetwork sizes = do
>     g <- getStdGen
>     return (nfromList sizes (randoms g))


> randomNetworks :: RandomGen g => g -> [Int] -> [Network]
> randomNetworks g sizes =
>     let length = sum $ zipWith (*) (drop 1 sizes) (map (+1) sizes)
>         weights = partitionEvery length (randoms g)
>     in map (nfromList sizes) weights



Breaking down networks
----------------------

> nflatten :: Network -> [Double]
> nflatten = concat . concat . ntoLists

> ntoLists :: Network -> [[[Double]]]
> ntoLists network = map toLists network



Using a network
---------------

> forward :: Network -> Matrix Double -> Matrix Double
> forward network input = foldl f input network
>     where f last next = let last' = last ||| (((rows last)><1) (repeat 1))
>             in cmap sigmoid (last' <> next)


Training a network
------------------

> fitness :: TrainingSet -> Network -> Double
> fitness training network =
>     let xs = map fst training
>         ys = concat (map (toList . flatten . snd) training)
>         actual = concat (map (toList . flatten . (forward network)) xs)
>         differences = zipWith (-) ys actual
>     in sqrt (sum (map (^2) differences))

