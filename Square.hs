import Genetics
import Neural
import Utilities
import Numeric.LinearAlgebra
import System.Random


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
    return $ map nflatten $ take 10 $ randomNetworks g [3, 4, 5, 6]

fit :: Genotype -> Double
fit = (fitness trainingSet) . (nfromList [3, 4, 5, 6])

-- Iterates fitness and selection on a population
iterations :: Population -> [Population]
iterations population = iterate (strangeSelection fit) population

-- Iterates fitness and selection on a population, where the
-- selection function uses randomness.
randomIterations :: RandomGen g => g -> Population -> [Population]
randomIterations g population = map snd $ iterate (rouletteWheel fit) (g, population)

maximums :: [Population] -> [Double]
maximums populations = map maximum $ (map.map) fit $ populations

-- example to see how well we are doing
example :: IO [Double]
example = do
    population <- randompopulation
    return $ maximums (iterations population)

main :: IO ()
main = do
    g <- getStdGen
    let population = map nflatten $ take 10 (randomNetworks g [3, 4, 5, 6])
    return ()
