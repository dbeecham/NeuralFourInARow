>> module Genetics where

Genetic Algorithms
==================


This module aims to implement basic genetic algoritms on fixed-length
lists. This includes a few different crossover-implementations (double-point,
triple-point, uniform) and a few different ways to select individuals, given
a fitness-function (including fitness-proportionate (also called Roulette Wheel),
tournament, elitism)) and some way to introduce mutation.

>> import Utilities
>> import Data.List
>> import System.Random



Genotypes and Population
------------------------

A genotype defines an individual (a solution) in the gene pool (the population,
or solution space). (A population, thus, is akin to a 'beam' in beam search.)
>> type Genotype = [Double]
>> type Population = [Genotype]


>> randomGenotypes :: (RandomGen g) => g -> Int -> [Genotype]
>> randomGenotypes g geneSize = partitionEvery geneSize (randoms g) 

>> randomPopulation :: RandomGen g => g -> Int -> Int -> Population
>> randomPopulation g populationSize geneSize = take populationSize (randomGenotypes g geneSize)




Selection
---------

This is a very strange selection indeed. It's only here for testing purposes.
(It's really bad, it gets stuck in local maxima *quickly*)
>> strangeSelection :: (Genotype -> Double) -> Population -> Population
>> strangeSelection fitness population = 
>>    let evaluations = add fitness population & sort & fmap snd
>>        noBadGenes = removeLast 2 evaluations
>>        children = crossover (3, 3) (car noBadGenes, cadr noBadGenes)
>>    in noBadGenes ++ [fst children, snd children]


A Roulette Wheel picks out

>> rouletteWheel :: RandomGen g => (Genotype -> Double) -> (Population, g) -> (Population, g)
>> rouletteWheel fitness (population, g) = (population', g')
>>     where fitnesses = fmap fitness population
>>           totalFitness = sum fitnesses
>>           relativeFitness = fmap (/totalFitness) fitnesses
>>           (rands, g') = nrandoms g (length population)


Identity selection. Only here for testing purposes.

>> selection :: Population -> Population
>> selection population = population



Crossover
---------

>> crossover :: (Int, Int) -> ([Double], [Double]) -> ([Double], [Double])
>> crossover (p1, p2) (xs, ys) =
>>     let r1 = (take p1 xs) ++ (take p2 (drop p1 ys)) ++ (drop (p1+p2) xs)
>>         r2 = (take p1 ys) ++ (take p2 (drop p1 xs)) ++ (drop (p1+p2) ys)
>>     in (r1, r2)
