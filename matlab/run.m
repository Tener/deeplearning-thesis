clear all; close all; clc;

addpath(genpath('..'));

%load games50k;
%games = games50000;
%load games163k_sparse;
%games = games_sparse;

games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games-all.sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games.raptor.700-inc-sparse.csv');
games_cnt = floor(size(games,1)/100)*100;
games = games(1:games_cnt,:);

train_x = games;

%%  ex1 train a 100 hidden unit RBM and visualize its weights
dbn.sizes = [100];
opts.numepochs =   5;
opts.batchsize = 100;
opts.momentum  =   0;
opts.alpha     =   1;
dbn = dbnsetup(dbn, train_x, opts);
dbn = dbntrain(dbn, train_x, opts);
% figure; visualize(dbn.rbm{1}.W', 1);   %  Visualize the RMB weights. this is because emacs is stupid: '

nn = dbnunfoldtonn(dbn, 10);
nn_applied = nnff(nn, games(1,:), zeros(1,10)); 
nnsave_to_file(nn);
