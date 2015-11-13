module Genetics where

import Utilities
import Data.List

type Genotype = [Double]
type Population = [Genotype]

strangeSelection :: (Genotype -> Double) -> Population -> Population
strangeSelection fitness population = 
    let evaluations = add fitness population & sort & map snd
        noBadGenes = removeLast 2 evaluations
        children = crossover (3, 3) (car noBadGenes, cadr noBadGenes)
    in noBadGenes ++ [fst children, snd children]


solutions :: Population -> [Population]
solutions population = iterate selection population

selection :: Population -> Population
selection population = population


crossover :: (Int, Int) -> ([Double], [Double]) -> ([Double], [Double])
crossover (p1, p2) (xs, ys) =
    let r1 = (take p1 xs) ++ (take p2 (drop p1 ys)) ++ (drop (p1+p2) xs)
        r2 = (take p1 ys) ++ (take p2 (drop p1 xs)) ++ (drop (p1+p2) ys)
    in (r1, r2)
