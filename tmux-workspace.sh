tmux new -d -s drifter -n repl
tmux send-keys -t repl 'cabal repl' C-m
tmux new-window -n emacs
tmux new-window -n bash
tmux attach -t drifter
