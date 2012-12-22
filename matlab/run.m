clear all; close all; clc;

addpath(genpath('/home/tener/dokumenty/ii/magisterka/DeepLearnToolbox'));

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games-all.sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games.raptor.700-inc-sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.raptor.1356116627.1000.csv');
%games_y = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.winnr.raptor.1356116627.1000.csv');

%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.2M.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data-good/games-sparse-random-simple-lvl1.csv');

games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.dense.2M.csv');
games_cnt = floor(size(games,1)/100)*100;
games = games(1:games_cnt,:);
train_x = games;
run_trainer(train_x);

games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.2M.csv');
games_cnt = floor(size(games,1)/100)*100;
games = games(1:games_cnt,:);
train_x = games;
run_trainer(train_x);


%games_y = games_y(1:games_cnt,:);
%train_y = games_y;
%run_trainer_ll(train_x, train_y);

