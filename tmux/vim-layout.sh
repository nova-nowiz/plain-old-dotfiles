#!/bin/sh 
tmux new-session \; \
  send-keys 'vim' C-m \; \
  split-window -h -p 10 \; \
  select-pane -t 0\;
