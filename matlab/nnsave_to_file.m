function nnsave_to_file(net)
    % open file
    filename = '/home/tener/nn.txt';
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
