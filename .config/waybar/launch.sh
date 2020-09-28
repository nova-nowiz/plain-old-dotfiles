#!/usr/bin/env bash

# Terminate already running bar instances
ps -ef | grep waybar$ | awk '{print $2}' | xargs kill -9

# Wait until the processes have been shut down
while pgrep -u $UID -x waybar >/dev/null; do sleep 1; done

# Launch waybar
waybar &

echo "Waybar launched..."
