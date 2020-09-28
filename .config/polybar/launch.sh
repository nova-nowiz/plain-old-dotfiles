#!/usr/bin/env bash

# Terminate already running bar instances
ps -ef | grep polybar\ mybar$ | awk '{print $2}' | xargs kill -9

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar mybar &

echo "Polybar launched..."
