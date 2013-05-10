{-# LANGUAGE ImplicitParams, Rank2Types, BangPatterns, OverloadedStrings #-}

module Main where

import Prelude hiding (putStr, putStrLn)

import AgentGeneric
import ConstraintsGeneric
import GenericGame
import GenericGameExperiments
import Matlab
import MinimalNN
import NeuralNets (parseNetFromFile)
import LittleGolem
import ThreadLocal

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Default
import Data.IORef
import Data.Text (Text)
import System.Directory
import Text.Printf

import qualified Control.Concurrent.Timeout as Timeout
import Data.Timeout

data ConstraintSource = CS_Cache | CS_Generate | CS_Gameplay

useCachedDBN = False
constraintSource = CS_Gameplay
searchTimeout = 5 # Minute
dbnGameCount = 100000
dbnGameProb = 0.3
dbnMatlabOpts = Just (def {dbnSizes = [250, 500]})

-- thresholds for global & local search, local search radius
thrG = 1          
thrL = 1          
localSearch = 0.002

getConstraintsPlayer :: FilePath -> Text -> ThrLocIO [(MyGame, MyGame)]
getConstraintsPlayer fp playerName = do
  records <- parseGameFileName fp
  let rec0 = filter ((playerName==) . playerWhite) records
      rec1 = filter (("1-0"==) . result) rec0
      rec2 = map moveSequence rec1
      constraints = concatMap (generateConstraintsGameplay P1) rec2
  printTL (length constraints)
  return (take 1000 constraints)

gameplayConstraints'0 = ("data-good/player_game_list_breakthrough_RayGarrison.txt", "Ray Garrison")
gameplayConstraints'1 = ("data-good/player_game_list_breakthrough_DavidScott.txt", "David Scott")

gameplayConstraints = gameplayConstraints'0

getConstraints :: ThrLocIO [(MyGame, MyGame)]
getConstraints = case constraintSource of
                   CS_Gameplay -> uncurry getConstraintsPlayer gameplayConstraints
                   CS_Generate -> genConstraints
                   CS_Cache    -> genConstraintsCached

main :: IO ()
main = runThrLocMainIO $ do
  printTL "Constraint generation"
  constraints <- getConstraints

  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  printTL ("DBN FN=",fn)

  let searchCB ref = (\ (bnNew,bsNew,acNew) -> do
                        let newTrim = (bnNew, bsNew)
                        updated <- atomicModifyIORef 
                                    ref (\ old@(_bnOld, !bsOld) -> do
                                                       if bsOld < bsNew
                                                        then (newTrim, True)
                                                        else (old,False))
                        when updated (printTL newTrim >> acNew)
                     )

  printTL "forever train last layer network & evaluate"
  let timestamp = getModificationTime "src/gg-exp3.hs"
  ts0 <- timestamp
  while ((==ts0) `fmap` timestamp) $ do
    threads <- getNumCapabilities
    bestRef <- newIORef (undefined, neginf)
    let withTimeout act = do
              asyncs <- act 
              let loop = do
                    best'0 <- snd `fmap` readIORef bestRef
                    delay
                    best'1 <- snd `fmap` readIORef bestRef
                    if best'0 == best'1 
                     then do
                       printTL "UNABLE TO IMPROVE, TIMEOUT"
                       readIORef bestRef 
                     else loop
                  delay = Timeout.threadDelay searchTimeout
              timeoutAsync <- async loop
              return (timeoutAsync:asyncs)

    (dbn, _) <- parseNetFromFile `fmap` readFile fn 
    let constraintsPacked = map packConstraint $ concatMap (uncurry generateConstraintsSimple) constraints
        packConstraint c = fmap packGame c
        packGame game = computeTNetworkSigmoid dbn $ reprToNN $ toRepr game

    printTL ("Total coinstraint count", length constraintsPacked)

    (_, _best) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronRandomReprSearch (searchCB bestRef) thrG thr constraintsPacked)) [1..threads])
    (_, bestLocal) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch thrL thr constraintsPacked)) [1..threads])

    putStrLnTL $ printf "FINAL SCORE %s" (show $ snd bestLocal)

    printTL "BEGIN EVALUATE"

    let llNetwork = uncurry mkTNetwork (fst bestLocal)
        evalNetwork = appendNetwork dbn llNetwork
        
    agSmpl <- mkTimed "simple" evalNetwork :: IO (AgentTrace AgentSimple)
    agTree <- mkTimed "tree" (evalNetwork, 3) :: IO (AgentTrace AgentGameTree)
    agMtcNet <- mkTimed "mtcNet" (1.1, 30, evalNetwork) :: IO (AgentTrace (AgentParMCTS AgentSimple))
    
    agRnd <- mkTimed "random" () :: IO (AgentTrace AgentRandom)
    agMTC <- mkTimed "mcts" 30 :: IO (AgentTrace AgentMCTS)

    putStrLnTL "======================================================================================"
    reportWin agSmpl agMTC P1
    reportWin agMtcNet agMTC P1
    reportWin agTree agMTC P1
    putStrLnTL "======================================================================================"

    return ()

  printTL "Source file changed, exiting"
  