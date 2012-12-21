function run_trainer_ll(train_x, train_y)
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
    
end;