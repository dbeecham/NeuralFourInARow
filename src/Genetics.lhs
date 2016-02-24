> {-# LANGUAGE NoImplicitPrelude, OverloadedLists #-}
> module Genetics where

Genetic Algorithms
==================


This module aims to implement basic genetic algoritms on fixed-length
lists. This includes a few different crossover-implementations (double-point,
triple-point, uniform) and a few different ways to select individuals, given
a fitness-function (including fitness-proportionate (also called Roulette Wheel),
tournament, elitism)) and some way to introduce mutation.

> import BasePrelude hiding (take)
> import Utilities hiding ((&))
> import System.Random
> import Control.Monad.Random
> import Data.Sequence (Seq, take, unstableSortBy)
> import qualified Data.Sequence as Sequence
>   (fromList)



Genotypes and Population
------------------------

A genotype defines an individual (a solution) in the gene pool (the population,
or solution space). (A population, thus, is akin to a 'beam' in beam search.)

> type Genotype = Seq Double
> type Population = Seq Genotype
> type FitnessFunction = Genotype -> Double

> type Rnd = Rand StdGen

> genotype :: Int -> Rnd Genotype
> genotype length = do
>   rnds <- sequence (replicate length getRandom)
>   return $ Sequence.fromList rnds

> population :: Int -> Int -> Rnd Population
> population populationSize geneLength = do
>   population <- sequence (replicate populationSize (genotype geneLength))
>   return $ Sequence.fromList population



Selection
---------

Elite selection picks out the best n individuals of a population.

> eliteSelection :: Int -> FitnessFunction -> Population -> Rnd Population
> eliteSelection x fitness population = do
>   return $ take x $ unstableSortBy (comparing fitness) population



A roulette wheel (also called Fitness proportionate selection) selects a single
individual randomly, with each individual having a probability proportional to
it's fitness of being selected.

 rouletteWheel :: RandomGen g => (Genotype -> Double) -> 
                               (Population, g) -> 
                               (Genotype, g)
 rouletteWheel fitness (population, g) = (population', g')
     where fitnesses = fmap fitness population
           totalFitness = sum fitnesses
           relativeFitness = fmap (/totalFitness) fitnesses
           (population', g') = pick g (zip relativeFitness population)


 rouletteWheeln :: RandomGen g => Int -> 
                                (Genotype -> Double) -> 
                                (Population, g) -> 
                                (Population, g)
 rouletteWheeln 0 _ (population, g) = ([], g)
 rouletteWheeln n fitness (population, g) =
   let (individual, g') = rouletteWheel fitness (population, g)
       (rest, g'') = rouletteWheeln (n - 1) fitness (population, g)
   in (individual:rest, g'')


 mySelection :: RandomGen g => (Genotype -> Double) -> (Population, g) -> (Population, g)
 mySelection fitness (population, g) =
   let elites = eliteSelection 2 fitness population
       geneLength = length (head elites)
       (eliteChildren, g2) = rndcrossover g (car elites, cadr elites)
       (rest, g3) = rouletteWheeln (length population - 4) fitness (population, g2)
       (children, g4) = rndcrossovers g3 rest
   in ((fst eliteChildren):(snd eliteChildren):(elites ++ children), g3)


Identity selection. Only here for testing purposes.

selection :: Population -> Population
selection population = population



Crossover
---------

 crossover :: (Int, Int) -> (Genotype, Genotype) -> (Genotype, Genotype)
 crossover (p1, p2) (xs, ys) =
     let r1 = (take p1 xs) ++ (take p2 (drop p1 ys)) ++ (drop (p1+p2) xs)
         r2 = (take p1 ys) ++ (take p2 (drop p1 xs)) ++ (drop (p1+p2) ys)
     in (r1, r2)


 rndcrossover :: RandomGen g => g -> (Genotype, Genotype) -> ((Genotype, Genotype), g)
 rndcrossover g (xs, ys) =
   let geneLength = min (length xs) (length ys)
       (crossoverPoint1, g1) = randomR (1, geneLength `div` 2) g
       (crossoverPoint2, g2) = randomR (1, geneLength `div` 2) g1
   in (crossover (crossoverPoint1, crossoverPoint2) (xs, ys), g2)

 rndcrossovers :: RandomGen g => g -> Population -> (Population, g)
 rndcrossovers g population =
   let marriages = pairs2 population
       (children, g2) = rndcrossovers' g marriages
   in (flattentuples children, g2)

 rndcrossovers' :: RandomGen g => g -> [(Genotype, Genotype)] -> ([(Genotype, Genotype)], g)
 rndcrossovers' g [] = ([], g)
 rndcrossovers' g ((a, b):rest) =
   let (children, g2) = rndcrossover g (a, b)
       (rest', g3) = rndcrossovers' g rest
   in (children:rest', g3)


Mutation
--------

mutation changes a random value in the genotype to a random value. This
happens with probability 'prob'. The nested 'let' is to decrease the
number of random values generated.

 mutation :: RandomGen g => g -> Double -> Genotype -> (Genotype, g)
 mutation g prob individual =
   let (roll, g') = random g
       willmutate = roll < prob
   in if willmutate
       then 
           let (mutationPoint, g'') = randomR (0, (length individual)) g'
               (mutationValue, g''') = random g''
           in (changeito individual mutationPoint mutationValue, g''')
       else
           (individual, g')
