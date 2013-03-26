{-# LANGUAGE OverloadedStrings, QuasiQuotes, ImplicitParams #-}

module Matlab where

import Data.String.Interpolation
import Data.String
import Data.Monoid
import System.FilePath

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
run :: (IsString a, Monoid a) => String -> a
run inputdata = [str|

clear all; close all; clc;

addpath(genpath('/home/tener/dokumenty/ii/magisterka/DeepLearnToolbox'));

games = dlmread('$fromString inputdata$');
games_cnt = floor(size(games,1)/100)*100;
games = games(1:games_cnt,:);
run_trainer(games);

% fixme: use that code
%games_y = games_y(1:games_cnt,:);
%train_y = games_y;
%run_trainer_ll(train_x, train_y);

|]

run_trainer_ll :: (IsString a, Monoid a, ?dbnSize :: Int, ?numEpochs :: Int) => String -> a
run_trainer_ll filepath = 
    let dbnsize = ?dbnSize :: Int
        numepochs = ?numEpochs :: Int
    in [str| 
function run_trainer_ll(train_x, train_y)
    dbn.sizes = [$:dbnsize$];
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
    nnsave_to_file_full(nn_ll,'$fromString filepath$');
    
end; |]

run_trainer :: (IsString a, Monoid a, ?dbnSize :: Int) => String -> a
run_trainer filepath = let dbnSize = ?dbnSize :: Int in [str|
function dbn = run_trainer(train_x)
    dbn.sizes = [$:dbnSize$];
    opts.numepochs =   5;
    opts.batchsize = 100;
    opts.momentum  =   0;
    opts.alpha     =   1;
    dbn = dbnsetup(dbn, train_x, opts);
    dbn = dbntrain(dbn, train_x, opts);
    nn = dbnunfoldtonn(dbn, 10);
    nnsave_to_file(nn, '$fromString filepath$');

end
|]