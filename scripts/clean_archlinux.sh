#!/bin/env bash

# clean cache packages
sudo pacman -Sc

# removed unused packages (orphans)
sudo pacman -Rns $(sudo pacman -Qtdq)

# remove user cache
echo "Removing user cache folder"
rm -r ~/.cache/*

exit 0
