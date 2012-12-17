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


class AgentMutable a where
    mutate :: Double -> a -> IO a

instance AgentMutable AgentNN where
    mutate = error "TODO: implement"

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
                               }

type Score = Double

data TournamentRules = TournamentRules { versusGames :: Int -- number of games played in versus games
                                       }

-- default instances

instance Default TournamentRules where
    def = TournamentRules { versusGames = 10 }

instance Default EvolutionParameters where
    def = EvolutionParameters { populationSize = 100
                              , championSquad = 3
                              , deathProbability = 0.3
                              , mutationForce = 0.1
                              , stepsToGo = 100
                              , plateuDetector = ()
                              , generator = Hidden (error "need to make generator :: GenIO first")
                              }

----- Simplified code

-- | run a single game for maximum given number of turns. takes the color of current player.
playVersus :: (Agent a, Agent b) => Color -> Int -> Int -> Board -> a -> b -> IO Board
playVersus color cutoff cnt brd a'fst a'snd | (isFinished brd || cnt == cutoff) = do
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
  -- unless (brd'new `elem` getMoves color brd) (error ("Invalid move by player: " ++ show color))

  putStrLn (printf "[%d] black: %d white: %d" cutoff (countBlack brd'new) (countWhite brd'new))
  playVersus (negColor color) cutoff (cnt+1) brd'new a'snd a'fst

-- run simple single game, AgentRandom vs. AgentNN
simpleGame :: Int -> IO Board
simpleGame cutoff = do
  ag'black <- mkAgent Black
  ag'white <- mkAgent White

  playVersus White cutoff 1 starting'board'default (ag'white :: AgentRandom) (ag'black :: AgentNN)

------------------------------

-- | run a single round of tournament. players play versus each one.
tournament :: (Agent a) => [a] -> IO [(Score,a)]
tournament agents = return (zip (cycle [0]) agents)

-- | play a game versus AgentRandom a number of times. receive score based on a number of games won and number of moves needed to win.
playVersusRandom :: (Agent a) => a -> IO (Score,Score)
playVersusRandom a = return (0,0)

-- | remove members of population that die. RIP.
cleanPopulation :: EvolutionParameters -> Population a -> IO (Population a)
cleanPopulation = error "TODO: implement"

-- | mutate a random member of population until we reach populationSize.
mutatePopulation :: (AgentMutable a) => EvolutionParameters -> Population a -> IO (Population a)
mutatePopulation = error "TODO: implement"

-- | apply tournament evalution for population
tournamentPopulation :: Population a -> IO (Population a)
tournamentPopulation = error "TODO: implement"

-- | run a single evolution step
evolutionStep :: (Agent a, AgentMutable a) => EvolutionParameters -> Population a -> IO (Population a)
evolutionStep params population = do
  clean <- cleanPopulation params population
  muts <- mutatePopulation params population
  toured <- tournamentPopulation muts

  return toured

-- | initialize parts that need IO monad
fillIOGaps :: EvolutionParameters -> IO EvolutionParameters
fillIOGaps params = return params

-- | create initial population of a given type
mkInitialPopulation :: (Agent a, AgentMutable a) => EvolutionParameters -> IO (Population a)
mkInitialPopulation params = error "TODO: implement"

-- | run evolution algorithm. without given population will start with fresh one.
evolution :: (Agent a, AgentMutable a) => (Maybe (Population a)) -> EvolutionParameters -> IO (Population a)
evolution initial'population initial'params  = error "TODO: implement"

---- printing, reporting etc.

prettyReportPopulation :: (Population a) -> IO ()
prettyReportPopulation _ = error "TODO: implement"
  
