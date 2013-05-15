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
import ConstraintsGA

import Control.Concurrent
import Control.Concurrent.Async
import Control.Arrow
import Control.Monad
import Data.Default
import Data.IORef
import Data.Text (Text)
import System.Directory
import Text.Printf

import qualified Control.Concurrent.Timeout as Timeout
import Data.Timeout

data ConstraintSource = CS_Cache | CS_Generate | CS_Gameplay

useCachedDBN = False || nullDBN
nullDBN = False
constraintSource = CS_Gameplay
searchTimeout = 30 # Second
dbnGameCount = 100000
dbnGameProb = 0.1
dbnMatlabOpts = Just (def {dbnSizes = [100]})
playerUseCoinstraints = 300

-- thresholds for global & local search, local search radius
thrG = 1          
thrL = 1          
localSearch = 0.0001

getConstraintsPlayer :: FilePath -> Text -> ThrLocIO [(MyGame, MyGame)]
getConstraintsPlayer fp playerName = do
  records <- parseGameFileName fp
  let rec0 = filter ((playerName==) . playerWhite) records
      rec1 = filter (("1-0"==) . result) rec0
      rec2 = map moveSequence rec1
      constraints = concatMap (generateConstraintsGameplay P1) rec2
  -- mapM_ printTL (map (toRepr *** toRepr) constraints)
  printTL ("Total constraints pairs read",length constraints)
  return (take playerUseCoinstraints constraints)

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

  

  let searchCB ref = (\ (!bnNew,!bsNew,!acNew) -> do
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
                     else do
                       printTL "Improvement found, timeout postponed"
                       loop
                  delay = Timeout.threadDelay searchTimeout
              handlerVar <- newEmptyMVar
              let handler = do
                                installUser1 (readIORef bestRef >>= putMVar handlerVar)
                                val <- takeMVar handlerVar
                                printTL "USR1 received, interrupting."
                                return val
              sigHandlerAsync <- async handler
              timeoutAsync <- async loop
              return (sigHandlerAsync:timeoutAsync:asyncs)

    let getDBNFile = (fst . parseNetFromFile) `fmap` readFile fn
        getDBNNull = return (mkTNetwork [] [])
    dbn <- if nullDBN then getDBNNull else getDBNFile
    let constraintsPacked = map packConstraint $ concatMap (uncurry generateConstraintsSimple) constraints
        packConstraint c = fmap packGame c
        packGame game = computeTNetworkSigmoid dbn $ reprToNN $ toRepr game

        asyncTL thr as = let old = ?thrLoc in
                         let ?thrLoc = old { tl_ident = show thr } in
                         async as

    printTL ("Total coinstraint count", length constraintsPacked)
    

    (_, _best2) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> asyncTL thr (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr thrL constraintsPacked)) [1..threads])
    printTL "some async singleNeuronMinimalGAReprSearch finished"
    (_, _best) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> asyncTL thr (singleNeuronRandomReprSearch (searchCB bestRef) thrG thr constraintsPacked)) [1..threads])
    (_, bestFinal) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> asyncTL thr (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch thrL (thr*2) constraintsPacked)) [1..threads])

    putStrLnTL $ printf "FINAL SCORE %s" (show $ snd bestFinal)

    printTL "BEGIN EVALUATE"

    let llNetwork = uncurry mkTNetwork (fst bestFinal)
        evalNetwork = appendNetwork dbn llNetwork
        
    agSmpl <- mkTimed "simple" evalNetwork :: IO (AgentTrace AgentSimple)
    agTree <- mkTimed "tree" (evalNetwork, 3) :: IO (AgentTrace AgentGameTree)
    agMtcNet <- mkTimed "mtcNet" (1.1, 5, evalNetwork) :: IO (AgentTrace (AgentParMCTS AgentSimple))
    
    agRnd <- mkTimed "random" () :: IO (AgentTrace AgentRandom)
    agMTC <- mkTimed "mcts" 30 :: IO (AgentTrace AgentMCTS)

    putStrLnTL "======================================================================================"
    --reportWin agMtcNet agMTC P1
    reportWin agSmpl agMTC P1
    reportWin agTree agMTC P1
    putStrLnTL "======================================================================================"

    return ()

  printTL "Source file changed, exiting"
  