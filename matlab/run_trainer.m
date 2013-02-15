function dbn = run_trainer(train_x)

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
