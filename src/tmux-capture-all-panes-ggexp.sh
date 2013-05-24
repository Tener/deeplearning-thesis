#!/bin/zsh

for pane in `tmux list-pane -a -F "#S:#I" | grep GG-EXP`; do
    tmux capture-pane -t $pane -p | tail -n 10 ;
    echo '========================================';
done

