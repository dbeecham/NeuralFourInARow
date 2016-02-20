module Square where

import Genetics
import qualified Neural
import Utilities
import qualified Numeric.LinearAlgebra
    ( cmap )
import Numeric.LinearAlgebra
    ( (<>)
    , (><)
    )
import System.Random

newtype Phenotype = Network Double

trainingSet :: TrainingSet
trainingSet = [ ((1><3)[1, 1, 1], (6><1)[1, 1, 1, 0, 1, 1])
              , ((1><3)[0, 0, 1], (6><1)[0, 0, 0, 0, 0, 1])
              , ((1><3)[0, 1, 0], (6><1)[0, 0, 0, 1, 0, 0])
              , ((1><3)[0, 1, 1], (6><1)[0, 0, 1, 0, 0, 1])
              , ((1><3)[1, 0, 0], (6><1)[0, 1, 0, 0, 0, 0])
              ]

randompopulation :: IO [Genotype]
randompopulation = do
    g <- getStdGen
    return $ map Neural.flatten $ take 10 $ randomNetworks g [3, 6]

morphogenesis :: Genotype -> Phenotype
morphogenesis g = Neural.fromList [3, 6]

fit :: Genotype -> Double
fit = (fitness trainingSet) . morphogenesis

-- Iterates fitness and selection on a population, where the
-- selection function uses randomness.
iterations :: RandomGen g => g -> Population -> [Population]
iterations g population = 
    map fst $ iterate (mySelection fit) (population, g)

maximums :: [Population] -> [Double]
maximums populations = map maximum $ (map.map) fit $ populations

-- example to see how well we are doing
example :: IO [Double]
example = do
    g <- newStdGen
    population <- randompopulation
    return $ maximums (iterations g population)

fitness :: Neural.TrainingSet -> Neural.Network -> Double
fitness training network =
    let xs = map fst training
        ys = concat (map (Neural.toList . Neural.flatten . snd) training)
        actual = concat (map (Neural.toList . Neural.flatten . (Neural.forward network)) xs)
        differences = zipWith (-) ys actual
    in (-1) * sqrt (sum (map (^2) differences))
