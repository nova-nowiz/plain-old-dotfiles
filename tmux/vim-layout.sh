#!/bin/sh 
tmux kill-session -a
tmux new-session -c ~/Documents/code/Game/src \; \
  send-keys 'vim' C-m \; \
  split-window -h -p 35 -c ~/Documents/code/Game/build \; \
  select-pane -t 0\;
