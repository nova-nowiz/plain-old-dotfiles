#!/bin/sh

if [ $(checkupdates | wc -l) -gt 0 ]
    then pacu
    else zsh
fi
