clear all; close all; clc;

addpath(genpath('/home/tener/dokumenty/ii/magisterka/DeepLearnToolbox'));

%load games50k;
%games = games50000;
%load games163k_sparse;
%games = games_sparse;
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games-all.sparse.csv');
%games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/games.raptor.700-inc-sparse.csv');
games = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.spars.raptor.1356116627.1000.csv');
games_y = dlmread('/home/tener/dokumenty/ii/magisterka/abalone/data/trace.winnr.raptor.1356116627.1000.csv');

%games_cnt = floor(size(games,1)/100)*100;
games_cnt = 700000;
games = games(1:games_cnt,:);
games_y = games_y(1:games_cnt,:);

train_x = games;
train_y = games_y;

run_trainer(train_x);
%run_trainer_ll(train_x, train_y);

