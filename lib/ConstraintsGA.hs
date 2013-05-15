{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

module ConstraintsGA where

import Prelude hiding (putStr, putStrLn)

-- import GenericGame
-- import AgentGeneric
import MinimalNN hiding (weights)
-- import NeuralNets (parseNetFromFile)
import ConstraintsGeneric
import ThreadLocal

import Numeric.Container (sumElements)
import Data.Packed.Vector (Vector)
import qualified Data.Packed.Vector as Vector

-- import Control.Monad
-- import Data.IORef
import System.Random
import Text.Printf
import Data.List (sortBy)
import Data.Ord (comparing)
import GA (Entity, GAConfig(..))
import qualified GA


-- | newtype representing sigle NN neuron without bias
newtype NN1 = NN1 { getNeuron :: SingleNeuron } deriving (Eq, Ord, Read, Show)


singleNeuronGAReprSearch :: ((SingleNeuron, Double, IO ()) -> IO a) -- ^ callback
                         -> Int                                     -- ^ thread number (modifies mutation strength)
                         -> Double                                  -- ^ target score (higher better)
                         -> [Constraint (Vector Double)]            -- ^ constraint list
                         -> ThrLocIO (SingleNeuron, Double)         -- ^ @(best'neuron, best'score)@ pair
singleNeuronGAReprSearch callback thrNum targetScore constraints = do
  let lastLayerSize :: Int
      lastLayerSize = case head constraints of
                        CBetter c _ -> length (Vector.toList c)
                        CBetterAll c _ -> length (Vector.toList c)

      mutRange = let r = 1 + fromIntegral thrNum :: Double in ((negate r), r)

      pickBest arch = head $ filter (\ (ms, _) -> ms /= Nothing) $ sortBy (comparing (fst)) arch

      config = GAConfig 
                    40 -- population size
                    5  -- archive size (best entities to keep track of)
                    200 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of replaced weights)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

  let loop = do
        stdgen <- newStdGen
        archive <- GA.evolve stdgen config (lastLayerSize, mutRange) constraints
        let best = pickBest archive
            ((Just sc),ent) = best
            nscore = negate sc
            neuron = getNeuron ent
            cb = do
                  putStrLnTL (printf "[%d] NEURON %s" thrNum (show neuron))
                  let ccount = length constraints
                  putStrLnTL (printf "[%d] GASCORE %f (cnum=%d, bad=%d)" thrNum nscore ccount (round $ fromIntegral ccount * (1-nscore) :: Int))
        _ <- callback (neuron, nscore, cb)
        if (nscore < targetScore) then loop else return (neuron,nscore)

  loop

mkSingleNeuron :: [Double] -> NN1
mkSingleNeuron weights = NN1 ([[weights]], [[0]])

getSingleNeuronWeights :: NN1 -> [Double]
getSingleNeuronWeights (NN1 ([[weights]], _)) = weights
getSingleNeuronWeights _ = error "getSingleNeuronWeights: unexpected input value"

instance Entity NN1    -- entity 
                Double -- score
                [Constraint (Vector Double)] -- data for scoring
                (Int, (Double, Double)) -- number of inputs for neuron, mutation 'range': value drawn from this range is multiplied with previous value
                IO -- monad to operate in
         where
   
  genRandom (count,_) seed = return (mkSingleNeuron $ take count (randomRs (-1,1) (mkStdGen seed)))

  crossover _ _ seed e1 e2 = return $ Just $ mkSingleNeuron e'w
    where
      g = mkStdGen seed
      e1'w = getSingleNeuronWeights e1
      e2'w = getSingleNeuronWeights e2
      e'w = zipWith3 (\ !b !x !y -> if b then x else y) (randoms g) e1'w e2'w

  mutation (_, mutRange) param seed ent = return $ Just $ mkSingleNeuron new'ent
    where
      (g1,g2) = split (mkStdGen seed)
      doMutate = map (<param) (randoms g1)
      newValues = randomRs mutRange g2
      new'ent = zipWith3 (\ !b !x !y -> if b then x else x*y) doMutate (getSingleNeuronWeights ent) newValues

  score' constraints ent = Just $ negate $ scoreConstraints (sumElements . computeTNetworkSigmoid (uncurry mkTNetwork (getNeuron ent))) constraints

  isPerfect (_,s) = s == 0.0