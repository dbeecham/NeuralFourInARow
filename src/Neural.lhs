> {-# LANGUAGE OverloadedLists #-}

Neural Networks
===============

This module aims to implement creation and evaluation of basic feed-forward
neural networks, along with a way to optimize the network (through
backpropagation) to fit a training set.

> module Neural 
> ( Network
> , network
> , TrainingSet
> , evaluate
> ) where

> import Prelude hiding (zipWith, zip, drop, length, take)
> import qualified Numeric.LinearAlgebra as Matrix
>   ( fromLists
>   , cmap
>   , rows
>   , toLists
>   )
> import Numeric.LinearAlgebra
>   ( (<>)
>   , (|||)
>   , (<#)
>   , (#>)
>   , (<.>)
>   , norm_2
>   , vector
>   , Matrix
>   , Element
>   , Numeric
>   , Vector
>   , matrix
>   )
> import Data.Sequence
> import qualified Data.Sequence as Sequence (fromList)
> import Data.Foldable (toList)
> import System.Random
> import Utilities
> import Data.Monoid (mappend)

> type Error = String

A network is a sequence of weight-matrices. Each entry in the list is a single layer
in the network, each column of a matrix defines the weighs of a neuron.

> type Network = Seq (Matrix Double)

To train a network, it's useful to define a training set (x, f(x)) which
the network aims to fit.

> type TrainingSet = Seq (Vector Double, Vector Double)



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

> test :: Network
> test = case network [1,2,3] [1..1000] of
>   Right x -> x
>   Left _ -> []

> network :: Seq Int -> Seq Double -> Either Error Network
> network sizes weights =
>   let matrixSizes' = (flip zip) sizes (drop 1 sizes)
>       matrixSizes = fmap (\(i,j) -> (i, j+1)) matrixSizes'
>       neededWeights = fmap (\(a,b) -> a*b) matrixSizes
>       totalNeededWeights = sum neededWeights
>       availableWeights = length weights
>       weights' = take totalNeededWeights weights
>       partitions = fromList $ partition' (toList neededWeights) (toList weights')
>       mkMatrix (_,j) values = matrix j values
>   in if availableWeights >= totalNeededWeights
>       then Right $ zipWith mkMatrix matrixSizes partitions
>       else Left $ "Was not given enough weights."


Breaking down networks
----------------------

> serialize :: Network -> Seq Double
> serialize = Sequence.fromList . concat . concat . toLists

> toLists :: Network -> Seq [[Double]]
> toLists = fmap Matrix.toLists


Using a network
---------------

> evaluate :: Network -> Vector Double -> Vector Double
> evaluate network input = 
>   let accumulator last next = Matrix.cmap sigmoid (last #> (next `mappend` vector [1]))
>   in foldr accumulator input network

> error :: Network -> TrainingSet -> Seq Double
> error network training =
>     let xs = fmap fst training
>         ys = fmap snd training
>         actual = fmap (evaluate network) xs
>         differences = zipWith (-) actual ys
>     in fmap norm_2 differences

