{-# LANGUAGE OverloadedStrings, QuasiQutation #-}

module Matlab where

import Data.String.Interpolation

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
run = [str|clear all; close all; clc;

addpath(genpath('/home/tener/dokumenty/ii/magisterka/DeepLearnToolbox'));

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games-all.sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games.raptor.700-inc-sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.raptor.1356116627.1000.csv');

% good ones:
games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.1M.csv');
% games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.raptor.1360694313.2000.csv');
% games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.raptor.1360698773.300.csv');


%games_y = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.winnr.raptor.1356116627.1000.csv');

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games.raptor.1356188234.300-inc-sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games.raptor.1359361813.700-inc-sparse.csv');

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.2M.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data-good/games-sparse-random-simple-lvl1.csv');

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.dense.2M.csv');
%games_cnt = floor(size(games,1)/100)*100;
%games = games(1:games_cnt,:);
%%train_x = games;
%run_trainer(games);
%%clear all; close all; clc;

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data-good/games-sparse-random-lvl01.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.2M.csv');

games_cnt = floor(size(games,1)/100)*100;
games = games(1:games_cnt,:);
train_x = games;
run_trainer(train_x);
%clear all; close all; clc;


%games_y = games_y(1:games_cnt,:);
%train_y = games_y;
%run_trainer_ll(train_x, train_y);

 |]

run_trainer_ll = [str| function run_trainer_ll(train_x, train_y)
    dbn.sizes = [100];
    opts.numepochs =   5;
    opts.batchsize = 100;
    opts.momentum  =   0;
    opts.alpha     =   1;
    dbn = dbnsetup(dbn, train_x, opts);
    dbn = dbntrain(dbn, train_x, opts);
    % figure; visualize(dbn.rbm{1}.W', 1);   %  Visualize the RMB weights. this is because emacs is stupid: '

    nn_ll = dbnunfoldtonn(dbn, 1);
    opts.numepochs =   1;
    opts.batchsize = 100;
    nn_ll = nntrain(nn_ll, train_x, train_y, opts);
    nnsave_to_file_full(nn_ll,'/home/tener/nn_ll.txt');
    
end; |]


run_trainer = [str|function dbn = run_trainer(train_x)

    dbn.sizes = [500];
    opts.numepochs =   5;
    opts.batchsize = 100;
    opts.momentum  =   0;
    opts.alpha     =   1;
    dbn = dbnsetup(dbn, train_x, opts);
    dbn = dbntrain(dbn, train_x, opts);
    % figure; visualize(dbn.rbm{1}.W', 1);   %  Visualize the RMB weights. this is because emacs is stupid: '

    nn = dbnunfoldtonn(dbn, 10);
    nnsave_to_file(nn, strcat('/home/tener/nn_',int2str(size(train_x,2)),'.txt'));



end

|]