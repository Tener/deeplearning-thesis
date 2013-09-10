{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import System.Process
import System.Environment
import System.Exit
import Control.Applicative
import Control.Monad
import Data.Maybe

data Experiment = Experiment { mainPane :: FilePath
                             , watchCommands :: [Command]
                             , extraCommands :: [String]
                             , sessionName :: String
                             } deriving (Eq, Read, Show)

type Command = (String, String)

createSession exp = do
  sessionAlreadyRunning <- (ExitSuccess==) <$> rawSystem "tmux" ["has-session", "-t", (sessionName exp)]
  when sessionAlreadyRunning (void (rawSystem "tmux" ["kill-session", "-t", (sessionName exp)]))
  let newWindows =  concat [ [";", "new-window", "-n", name, cmd] | (name,cmd) <- watchCommands exp]
      -- detach = [";", "detach-client", "-s", (sessionName exp)]
      renameMain = [";", "rename-window", "-t", ":0", (mainPane exp)]
      newSession = ["new-session", "-s", (sessionName exp)] 
      args = (newSession
              ++ newWindows
              ++ renameMain
              ++ (extraCommands exp))

  rawSystem "tmux" args
  putStrLn (showCommandForUser "tmux" args)

  -- putStrLn $ "Session launched: " ++ sessionName exp ++ " | tmux attach -t " ++ sessionName exp

ggExp5 = Experiment "main" [("won games", "tail -n 1000000 -f g5.txt | grep --color=always \"won \"")
                           ,("scores", "tail -n 1000000 -f g5.txt | grep --color=always SCORE")
                           ,("wins", "tail -n 1000000 -f g5.txt | grep --color=always WINS")
                           ,("multi", "tail -n 1000000 -f g5.txt | grep --color=always multiNeuron")
                           ,("killall", "zsh")
                           ,("console", "zsh")
                           ]
                    [";", "send-keys", "-t", "killall", ": killall -USR1 gg-exp5\n",
                     ";", "send-keys", "-t", "main", "zsh\nwhile (true) { make && make run-g5 }\n"]

                    "GG-EXP5"

ggExp6 = Experiment "main" [("won games", "tail -n 1000000 -f g6.txt | grep --color=always \"won \"")
                           ,("scores", "tail -n 1000000 -f g6.txt | grep --color=always SCORE")
                           ,("wins", "tail -n 1000000 -f g6.txt | grep --color=always WINS")
                           ,("multi", "tail -n 1000000 -f g6.txt | grep --color=always multiNeuron")
                           ,("killall", "zsh")
                           ,("console", "zsh")
                           ]
                    [";", "send-keys", "-t", "killall", ": killall -USR1 gg-exp6\n",
                     ";", "send-keys", "-t", "main", "zsh\nwhile (true) { make && make run-g6 }\n"]

                    "GG-EXP6"

ggExp7 = Experiment "main" [("won games", "tail -n 1000000 -f g7.txt | grep --color=always \"won \"")
                           ,("scores", "tail -n 1000000 -f g7.txt | grep --color=always SCORE")
                           ,("wins", "tail -n 1000000 -f g7.txt | grep --color=always WINS")
                           ,("multi", "tail -n 1000000 -f g7.txt | grep --color=always multiNeuron")
                           ,("killall", "zsh")
                           ,("console", "zsh")
                           ]
                    [";", "send-keys", "-t", "killall", ": killall -USR1 gg-exp7\n",
                     ";", "send-keys", "-t", "main", "zsh\nwhile (true) { make && make run-g7 }\n"]

                    "GG-EXP7"

ggExp8 = Experiment "main" [("won games", "tail -n 1000000 -f g8.txt | grep --color=always \"won \"")
                           ,("scores", "tail -n 1000000 -f g8.txt | grep --color=always SCORE")
                           ,("wins", "tail -n 1000000 -f g8.txt | grep --color=always WINS")
                           ,("multi", "tail -n 1000000 -f g8.txt | grep --color=always multiNeuron")
                           ,("killall", "zsh")
                           ,("console", "zsh")
                           ]
                    [";", "send-keys", "-t", "killall", ": killall -USR1 gg-exp8\n",
                     ";", "send-keys", "-t", "main", "zsh\nwhile (true) { make && make run-g8 }\n"]

                    "GG-EXP8"

ggExp9 = Experiment "main" [("won games", "tail -n 1000000 -f g9.txt | grep --color=always \"won \"")
                           ,("scores", "tail -n 1000000 -f g9.txt | grep --color=always SCORE")
                           ,("wins", "tail -n 1000000 -f g9.txt | grep --color=always WINS")
                           ,("multi", "tail -n 1000000 -f g9.txt | grep --color=always multiNeuron")
                           ,("killall", "zsh")
                           ,("console", "zsh")
                           ]
                    [";", "send-keys", "-t", "killall", ": killall -USR1 gg-exp9\n",
                     ";", "send-keys", "-t", "main", "zsh\nwhile (true) { make && make run-g9 }\n"]

                    "GG-EXP9"

ggExp10 = Experiment "main" [("won games", "tail -n 1000000 -f g10.txt | grep --color=always \"won \"")
                           ,("scores", "tail -n 1000000 -f g10.txt | grep --color=always SCORE")
                           ,("wins", "tail -n 1000000 -f g10.txt | grep --color=always WINS")
                           ,("multi", "tail -n 1000000 -f g10.txt | grep --color=always multiNeuron")
                           ,("exp10", "tail -n 1000000 -f g10.txt | grep --color=always exp10")
                           ,("killall", "zsh")
                           ,("console", "zsh")
                           ]
                    [";", "send-keys", "-t", "killall", ": killall -USR1 gg-exp10\n",
                     ";", "send-keys", "-t", "main", "zsh\nwhile (true) { make && make run-g10 }\n"]

                    "GG-EXP10"

defaultSession = ggExp7

sessions = [("exp5", ggExp5)
           ,("exp6", ggExp6)
           ,("exp7", ggExp7)
           ,("exp8", ggExp8)
           ,("exp9", ggExp9)
           ,("exp10", ggExp10)
           ]

main = do
  args <- getArgs
  case args of
    [] -> createSession defaultSession
    [x] -> maybe (fail ("unable to find session with ID " ++ show x)) createSession (lookup x sessions)
    _ -> fail "provide either 0 arguments (default session) or 1 (specific single session)"
      
