module Tournament where

import Board
import Agent
import NeuralNets
import CommonDatatypes
import Utils

import Data.Default
import System.Random.MWC
import System.IO
import Text.Printf
import Control.Monad
import Data.List(sort)
import qualified Data.Vector.Unboxed as U

import Control.Concurrent
import Control.Concurrent.Async

class AgentMutable a where
    mutate :: GenIO -> Double -> a -> IO a

instance AgentMutable AgentNN where
    mutate g force ag = do
      let s = length w
          (b,w) = lastLayer ag
      reals'prb <- uniformVector g (s+1)
      reals'mod <- uniformVector g (s+1)
      
      let w'new = [ if (U.unsafeIndex reals'prb i) < force then val * 10 * (U.unsafeIndex reals'mod i) else val | (i,val) <- (zip [0..] w) ]
          b'new = if (U.unsafeIndex reals'prb s) < force then b * 10 * (U.unsafeIndex reals'mod s) else b

          ag'new = ag { lastLayer = (b'new,w'new) }

      return $! ag'new

data EvolutionParameters = EvolutionParameters { 
      -- single step parameters
      populationSize :: Int -- number of members stay alive at once
    , championSquad :: Int -- how many members stay alive no matter what
    , deathProbability :: Double -- how is it probable that a non-champion member dies 
    , mutationForce :: Double -- how strong is mutation
      -- meta parameters
    , stepsToGo :: Int -- how long will we run?
    , plateuDetector :: () -- TODO
      -- utilities
    , generator :: Hidden GenIO -- random generator -- for convienience
    }

data Population a = Population { members :: [(Score,a)] -- members of population
                               , championScore :: Score -- members above that score will be considered champions
                               } deriving (Eq, Show)

type Score = Double

--data TournamentRules = TournamentRules { versusGames :: Int -- number of games played in versus games
--                                       , maximumMoves :: Int -- maximum number of moves allowed in game before calling a draw
--                                       , versusRandomCount :: Int -- number of games played in versusRandom challenge
--                                       }

-- default instances

-- instance Default TournamentRules where
--     def = TournamentRules { versusGames = 10 }

instance Default EvolutionParameters where
    def = EvolutionParameters { populationSize = 5
                              , championSquad = 3
                              , deathProbability = 0.9
                              , mutationForce = 0.1
                              , stepsToGo = 100
                              , plateuDetector = ()
                              , generator = Hidden (error "need to make generator :: GenIO first")
                              }

----- Simplified code

-- | run a single game for maximum given number of turns. takes the color of current player.
playVersus :: (Agent a, Agent b) => Color -> Int -> Int -> Board -> a -> b -> IO Board
playVersus color'now cutoff cnt brd a'fst a'snd | (isFinished brd || cnt == cutoff) = do
  hPutStrLn stderr (printf "[%d] Game is finished after %d moves!" cutoff cnt)
  hPutStrLn stderr (printf "[%d] Winner: %s" cutoff (show (getWinner brd)))
  -- save finished board to svg
  -- saveBoard brd (printf "svg/finished-board-%d-%06d.svg" cutoff cnt)
  return brd

                     | otherwise = do
  brd'new <- makeMove a'fst brd
  -- save board every 1000 moves
  -- when (cnt `mod` 1000 == 0) (saveBoard brd (printf "svg/playing-board-%d-%06d.svg" cutoff cnt))

  -- sanity check -- disable for honest players
  -- unless (brd'new `elem` getMoves color'now brd) (error ("Invalid move by player: " ++ show color))

  putStrLn (printf "[%d] black: %d white: %d" cutoff (countBlack brd'new) (countWhite brd'new))
  playVersus (negColor color'now) cutoff (cnt+1) brd'new a'snd a'fst

-- run simple single game, AgentRandom vs. AgentNN
simpleGame :: Int -> IO Board
simpleGame cutoff = do
  ag'black <- mkAgent Black
  ag'white <- mkAgent White

  playVersus White cutoff 1 starting'board'default (ag'white :: AgentRandom) (ag'black :: AgentNN)

------------------------------

-- | run a single round of tournament. players play versus each one.
tournament :: (Eq a, Agent a) => [a] -> IO [(Score,a)]
tournament agents = do
  scored <- mapM (\ (i,ag) -> do
          score <- playVersusRandom ag
          putStrLn (printf "[%d/%d] tournament-pctWon: %f" (i::Int) (length agents) score)
          return (score, ag)
                  ) (zip [1..] agents)

  return scored

-- | play a game versus AgentRandom a number of times. receive score based on a number of games won.
playVersusRandom :: (Agent a) => a -> IO (Score)
playVersusRandom ag = do
  randomer <- mkAgent Black
  let cutoff = 300
      games :: (Num a) => a
      games = 5
  winners <- mapConcurrently (\ _ -> (playVersus White cutoff 1 starting'board'default ag (randomer :: AgentRandom))) [1..games]
  
  let won'w = count (Just White)
      won'b = count (Just Black)
      draw  = count Nothing
      count :: (Maybe Color) -> Int
      count x = length $ filter (==x) (map getWinner winners)
      pcnt'won = (fromIntegral won'w) / games
                
      killed'stones = games * 14 - (sum $ map countBlack winners)

      score = pcnt'won * 1000 + (fromIntegral killed'stones)

  return score

-- | remove members of population that die. RIP.
cleanPopulation :: EvolutionParameters -> Population a -> IO (Population a)
cleanPopulation params pop = do
  reals <- uniformVector (unhide $ generator $ params) (populationSize params)
  let p = deathProbability params
      c = championScore pop
      new'mem = map snd $ filter choose (zip [0..] (members pop))
      choose (i,el) = if fst el < c then (U.unsafeIndex reals i) < p else True
      new'pop = pop { members = new'mem }
  return new'pop

-- | mutate a random member of population until we reach populationSize.
mutatePopulation :: (AgentMutable a) => EvolutionParameters -> Population a -> IO (Population a)
mutatePopulation params pop = do
  let need = populationSize params - alive
      alive = length (members pop)
      g = unhide $ generator params
      pick = do
        i <- uniformR (0,alive-1) g
        return (snd (members pop !! i))
  agents'new <- replicateM need (pick >>= mutate g (mutationForce params))
  let mem = (members pop) ++ zip (cycle [0]) agents'new
      pop'new = pop { members = mem }
  return pop'new


-- | apply tournament evalution for population
tournamentPopulation :: (Eq a, Agent a, Ord a) => EvolutionParameters -> Population a -> IO (Population a)
tournamentPopulation params pop = do
  mems <- tournament (map snd $ members pop)
  let members'scored = reverse $ sort $ mems
      cscore = maximum (map fst members'scored)
  return (pop { members = members'scored,
                championScore = cscore })

-- | run a single evolution step
evolutionStep :: (Ord a, Agent a, AgentMutable a) => EvolutionParameters -> Population a -> IO (Population a)
evolutionStep params population = do
  clean <- cleanPopulation params population
  muts <- mutatePopulation params population
  toured <- tournamentPopulation params muts

  return toured

-- | initialize parts that need IO monad
fillIOGaps :: EvolutionParameters -> IO EvolutionParameters
fillIOGaps params = do
  g <- withSystemRandom $ asGenIO $ return
  return (params { generator = (Hidden g) })

-- | create initial population of a given type
mkInitialPopulation :: (Agent a, AgentMutable a) => EvolutionParameters -> IO (Population a)
mkInitialPopulation params = do
  agents <- replicateM (populationSize params) (mkAgent White)
  agents' <- mapM (mutate (unhide $ generator params) 2) agents
  return (Population (zip (cycle [0]) agents') (-1))
  
-- | run evolution algorithm. without given population will start with fresh one.
evolution :: (Ord a, Eq a, Show a, Agent a, AgentMutable a) => (Maybe (Population a)) -> EvolutionParameters -> IO (Population a)
evolution initial'population initial'params = do
  params <- fillIOGaps initial'params
  population <- case initial'population of
                  Just pop -> return pop
                  Nothing -> mkInitialPopulation params

  let go par pop = if stepsToGo par > 0 
                    then do
                     pop'new <- evolutionStep par pop
                     prettyReportPopulation pop'new
                     if (championScore pop) < 200
                      then print "RESET" >> evolution initial'population initial'params
                      else go (decSteps par) pop'new
                    else return pop

      decSteps par = par { stepsToGo = (stepsToGo par)-1 }

  go params population

---- printing, reporting etc.

prettyReportPopulation :: (Show a) => (Population a) -> IO ()
prettyReportPopulation pop = do
  print ("POPULATION",pop)
  print ("POPULATION-SCORE",championScore pop)
  
