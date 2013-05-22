{-# LANGUAGE FlexibleInstances, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module ConstraintsGA where

import Prelude hiding (putStr, putStrLn)

import MinimalNN hiding (weights)
import MinimalGA 
import ConstraintsGeneric
import ThreadLocal
import GenericGameExperiments

import Numeric.Container (sumElements)
import Data.Packed.Vector (Vector)
import qualified Data.Packed.Vector as Vector

import Control.Concurrent.Async
import Control.Monad
import System.Random
import System.Random.MWC
import Text.Printf
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Maybe
import Data.IORef

import GA (Entity, GAConfig(..))
import qualified GA

import Data.Timeout
-- import qualified Control.Concurrent.Timeout as Timeout


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

multiNeuronMinimalGAReprSearch :: Int                                     -- ^ thread count
                               -> Int                                     -- ^ allowed bad count
                               -> Timeout                                 -- ^ search timeout
                               -> Double                                  -- ^ target score (higher better)
                               -> [Constraint (Vector Double)]            -- ^ constraint list
                               -> ThrLocIO [(SingleNeuron, Double)]       -- ^ @(best'neuron, best'score)@ pair
multiNeuronMinimalGAReprSearch threads allowedBad searchTimeout singleNeuronTarget constraints = do
  let loop acc constraintsRemaining | length constraintsRemaining <= allowedBad = return acc
                                    | otherwise = do
        bestRef <- newIORef (undefined, neginf)

        let mconfig = Nothing -- Just (EvolveConfig 50 3 750 0.2 0.8 0.2)
            callback args@(_neuron, score, logger) = do
                    let continue = score < singleNeuronTarget
                    _ <- searchCB bestRef args
                    unless continue (do
                                      logger
                                      printTL ("multiNeuronMinimalGAReprSearch::callback", continue))
                    return continue
            
            wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])

        (_,(neuron, score)) <- wt (\ thr -> async $ singleNeuronMinimalGAReprSearch callback thr singleNeuronTarget constraintsRemaining mconfig)
        let predicate constraint = checkConstraint (sumElements . computeTNetworkSigmoid (uncurry mkTNetwork neuron)) constraint
            (constGood, constBad) = partition predicate constraintsRemaining
            result = (neuron, score)
        printTL ("multiNeuronMinimalGAReprSearch::loop step", score, length constBad, length constGood)
        loop (result:acc) constBad
  loop [] constraints

singleNeuronMinimalGAReprSearch :: ((SingleNeuron, Double, IO ()) -> IO Bool) -- ^ callback, return True means continue search
                                -> Int                                     -- ^ thread number (modifies mutation strength)
                                -> Double                                  -- ^ target score (higher better)
                                -> [Constraint (Vector Double)]            -- ^ constraint list
                                -> (Maybe (AlmostEvolveConfig NN1))        -- ^ maybe almost config
                                -> ThrLocIO (SingleNeuron, Double)         -- ^ @(best'neuron, best'score)@ pair
singleNeuronMinimalGAReprSearch callback thrNum targetScore constraints mConfig = do
  let lastLayerSize :: Int
      lastLayerSize = case head constraints of
                        CBetter c _ -> length (Vector.toList c)
                        CBetterAll c _ -> length (Vector.toList c)

      mutRange = let r = 1 + fromIntegral thrNum :: Double in ((negate r), r)

      config' = (fromMaybe (EvolveConfig
                            40 -- population size
                            10 -- archive size (best entities to keep track of)
                            2000 -- maximum number of generations
                            0.2 -- mutation rate (% of entities by mutation)
                            0.8 -- crossover rate (% of entities by crossover)
                            0.2 -- parameter for mutation (% of replaced weights))
                           ) mConfig) 

      config = config' (cbNew . esBest)

      unwrapResult (sc,ent) = (negate sc, getNeuron ent)

      cbNew :: (Double, NN1) -> ThrLocIO Bool
      cbNew best = do
        let (nscore,neuron) = unwrapResult best
            cbMaster = do
                  putStrLnTL (printf "[%d] NEURON %s" thrNum (show neuron))
                  let ccount = length constraints
                  putStrLnTL (printf "[%d] GASCORE %f (cnum=%d, bad=%d)" thrNum nscore ccount (round $ fromIntegral ccount * (1-nscore) :: Int))
        continue <- callback (neuron, nscore, cbMaster)
        return ((nscore < targetScore) && continue)
            
      loop = do
        rgen <- mkGen
        (best:_) <- MinimalGA.evolve config lastLayerSize (rgen, lastLayerSize, mutRange) constraints
        printTL "singleNeuronMinimalGAReprSearch: MinimalGA.evolve finished"
        continue <- cbNew best
        printTL ("singleNeuronMinimalGAReprSearch: continue", continue)
        if continue then loop else return $ swap $ unwrapResult best

  val <- loop
  printTL "singleNeuronMinimalGAReprSearch: loop finished"
  return val


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

instance MinimalGA NN1 where
    type Score NN1 = Double
    type ScoreDataset NN1 = [Constraint (Vector Double)]
    -- | size
    type EntityParams NN1 = Int
    -- | random gen, size again
    type WorkParams NN1 = (GenIO, Int, (Double,Double))

    newEntity (rgen,_,_) size = mkSingleNeuron `fmap` replicateM size (uniformR (-1,1) rgen)
    {-# INLINE newEntity #-}
    crossover (rgen,size,_) ent1 ent2 = do
       bools <- replicateM size (uniform rgen)
       let e1'w = getSingleNeuronWeights ent1
           e2'w = getSingleNeuronWeights ent2
           e3'w = zipWith3 (\ !b !x !y -> if b then x else y) bools e1'w e2'w
       return $ mkSingleNeuron e3'w
    {-# INLINE crossover #-}
    mutation (rgen,size,mutRange) mutForce ent = do
       bools <- map (<mutForce) `fmap` (replicateM size (uniformR mutRange rgen))
       values <- replicateM size (uniform rgen)
       let entNew'w = zipWith3 (\ !b !x !y -> if b then x else x*y) bools (getSingleNeuronWeights ent) values
       return $ mkSingleNeuron entNew'w
    {-# INLINE mutation #-}
    scoreEntity constraints ent = negate $ scoreConstraints (sumElements . computeTNetworkSigmoid (uncurry mkTNetwork (getNeuron ent))) constraints
    {-# INLINE scoreEntity #-}

