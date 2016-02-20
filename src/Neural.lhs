Neural Networks
===============

This module aims to implement creation and evaluation of basic feed-forward
neural networks, along with a way to optimize the network (through
backpropagation) to fit a training set.

> module Neural where

> import qualified Numeric.LinearAlgebra as Matrix
>   ( fromLists
>   , cmap
>   , rows
>   , toLists
>   )
> import Numeric.LinearAlgebra
>   ( (<>)
>   , (><)
>   , (|||)
>   , (<#)
>   , (#>)
>   , (<.>)
>   , vector
>   , Matrix
>   , Element
>   , Numeric
>   , Vector
>   )
> import System.Random
> import Utilities
> import Data.Monoid (mappend)

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

> sigmoid :: (Floating a) => a -> a
> sigmoid x = 1/(1 + exp(-x))

> sigmoid' :: (Floating a) => a -> a
> sigmoid' x = sigmoid x * (1 - sigmoid x)



Creating networks
-----------------

If we have a list of n*m matrixes (in list form), then we can

> fromLists :: [[[Double]]] -> Network
> fromLists = map Matrix.fromLists

> fromList :: [Int] -> [Double] -> Network
> fromList sizes values = 
>     let matrixSizes = map (\(x, y) -> (x + 1, y)) (pairs sizes)
>         multTuple (x, y) = x*y
>         partitions = partition' (map multTuple matrixSizes) values
>     in map2 (\(x, y) values -> (x><y) values) matrixSizes partitions



Breaking down networks
----------------------

> flatten :: Network -> [Double]
> flatten = concat . concat . toLists

> toLists :: Network -> [[[Double]]]
> toLists = map Matrix.toLists


Using a network
---------------

> evaluate :: Network -> Vector Double -> Vector Double
> evaluate network input = 
>   let inputWithBias = input `mappend` vector [1]
>       accumulator last next = Matrix.cmap sigmoid (last #> next)
>   in foldr accumulator input network

> error :: Network -> TrainingSet -> Vector Double
> error network training =
>     let xs = map fst training
>         ys = map snd training
>         actual = map (evaluate network) xs
>         differences = zipWith (-) actual ys
>     in sum $ map norm_2 differences

