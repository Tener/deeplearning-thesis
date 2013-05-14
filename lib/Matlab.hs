{-# LANGUAGE OverloadedStrings, QuasiQuotes, ImplicitParams #-}

module Matlab where

import Data.String.Interpolation
import Data.String
import Data.Monoid
import Data.Default
import System.FilePath
import System.Directory
import System.Process
import System.Exit

data MatlabImp = Matlab | Octave deriving (Eq, Ord, Read, Show)
data MatlabOpts = MatlabOpts { dbnSizes :: [Int], numEpochs :: Int, implementation :: MatlabImp } deriving (Eq, Ord, Read, Show)

instance Default MatlabOpts where
    def = MatlabOpts { dbnSizes = [100], numEpochs = 5, implementation = Octave }

prepAndRun :: MatlabOpts -> FilePath -> FilePath -> IO ExitCode
prepAndRun matlabOpts outputDirectory' inputDataFile' = do
  let ?dbnSizes = dbnSizes matlabOpts
      ?numEpochs = numEpochs matlabOpts

  outputDirectory <- canonicalizePath outputDirectory'
  inputDataFile <- canonicalizePath inputDataFile'

  createDirectoryIfMissing True outputDirectory

  writeFile (outputDirectory </> "nnsave_to_file_full.m") nnsave_to_file_full
  writeFile (outputDirectory </> "nnsave_to_file.m") nnsave_to_file
  writeFile (outputDirectory </> "run_main.m") (run_main inputDataFile)
  writeFile (outputDirectory </> "run_trainer.m") (run_trainer (outputDirectory </> "dbn.txt"))
  writeFile (outputDirectory </> "run_trainer_ll.m") (run_trainer_ll (outputDirectory </> "dbn-ll.txt"))

  procHandle <- case implementation matlabOpts of
                  Octave -> runProcess "octave" (words "run_main.m") (Just outputDirectory) Nothing Nothing Nothing Nothing 
                  Matlab -> runProcess "matlab" (words "-nosplash -nodisplay -r run_main") (Just outputDirectory) Nothing Nothing Nothing Nothing 
  exitCode <- waitForProcess procHandle
  return exitCode

-- zapisuje wszystkie warstwy, łącznie z ostatnią
nnsave_to_file_full :: (IsString a, Monoid a) => a
nnsave_to_file_full = [str|

function nnsave_to_file_full(net, filename)
    % open file
    file = fopen(filename, 'w');
    
    % write sizes in single row
    for i = 2 : net.n
       fprintf(file, '%d ', net.size(i)); 
    end
    fprintf(file, '\n');
    fclose(file);
    
    % write biases in 'layer count' rows
    biases = net.b;
    for i = 1 : net.n-1;
        dlmwrite(filename, biases{i}', 'delimiter', ' ', '-append'); % '
    end
    
    % write weights in 'neuron count' rows
    weights = net.W;
    for i = 1 : net.n-1;
        dlmwrite(filename, weights{i}, 'delimiter', ' ', '-append');
    end
end

 |]

-- zapisuje wszystkie warstwy poza ostatnią
nnsave_to_file :: (IsString a, Monoid a) => a
nnsave_to_file = [str| function nnsave_to_file(net, filename)
    % open file
    file = fopen(filename, 'w');
    
    % write sizes in single row
    for i = 2 : net.n-1
       fprintf(file, '%d ', net.size(i)); 
    end
    fprintf(file, '\n');
    fclose(file);
    
    % write biases in 'layer count' rows
    biases = net.b;
    for i = 1 : net.n-2;
        dlmwrite(filename, biases{i}', 'delimiter', ' ', '-append');
    end
    
    % write weights in 'neuron count' rows
    weights = net.W;
    for i = 1 : net.n-2;
        dlmwrite(filename, weights{i}, 'delimiter', ' ', '-append');
    end
end

|]

-- główny skrypt uruchamiąjący wszystkie pozostałe
run_main :: (IsString a, Monoid a) => String -> a
run_main inputdata = [str|

% clear all; close all; clc;

addpath(genpath('/home/tener/dokumenty/ii/magisterka/magisterka-deeplearning/DeepLearnToolbox'));

games = dlmread('$fromString inputdata$');
games_cnt = floor(size(games,1)/100)*100;
games = games(1:games_cnt,:);
run_trainer(games);

% fixme: use that code
%games_y = games_y(1:games_cnt,:);
%train_y = games_y;
%run_trainer_ll(train_x, train_y);

exit;

|]

run_trainer_ll :: (IsString a, Monoid a, ?dbnSizes :: [Int], ?numEpochs :: Int) => String -> a
run_trainer_ll outputFilepath = 
    let dbnsizes = ?dbnSizes
        numepochs = ?numEpochs :: Int
    in [str| 
function run_trainer_ll(train_x, train_y)
    dbn.sizes = $:dbnsizes$;
    opts.numepochs = [$:numepochs$];
    opts.batchsize = 100;
    opts.momentum  =   0;
    opts.alpha     =   1;
    dbn = dbnsetup(dbn, train_x, opts);
    dbn = dbntrain(dbn, train_x, opts);
    nn_ll = dbnunfoldtonn(dbn, 1);
    opts.numepochs =   1;
    opts.batchsize = 100;
    nn_ll = nntrain(nn_ll, train_x, train_y, opts);
    nnsave_to_file_full(nn_ll,'$fromString outputFilepath$');
    
end; |]

run_trainer :: (IsString a, Monoid a, ?numEpochs :: Int, ?dbnSizes :: [Int]) => String -> a
run_trainer outputFilepath = let dbnSizes' = ?dbnSizes 
                                 numepochs = ?numEpochs :: Int
                             in [str|
function dbn = run_trainer(train_x)
    dbn.sizes = $:dbnSizes'$;
    opts.numepochs = [$:numepochs$];
    opts.batchsize = 100;
    opts.momentum  =   0;
    opts.alpha     =   1;
    dbn = dbnsetup(dbn, train_x, opts);
    dbn = dbntrain(dbn, train_x, opts);
    nn = dbnunfoldtonn(dbn, 10);
    nnsave_to_file(nn, '$fromString outputFilepath$');


end
|]