#!/usr/bin/env bash

## Auto-Nitro is a script to automatically change the wallpaper with Nitrogenium

# Config
DELAY=300

while true; do
    # run nitrogenium.sh and sleep
    $HOME/.config/i3/randomWallpaper.sh &
    sleep $DELAY
done
