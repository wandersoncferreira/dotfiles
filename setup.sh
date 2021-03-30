#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "First arg must be 'reifyhealth'";
    exit 1
fi

echo "setup nix"

sudo ln -sf $HOME/repos/personal/dotfiles/nix/configuration.nix /etc/nixos/configuration.nix
sudo ln -sf $HOME/repos/personal/dotfiles/nix/configurations /etc/nixos/configurations
sudo ln -sf $HOME/repos/personal/dotfiles/nix/home-managers/$1.nix $HOME/.config/nixpkgs/home.nix 

echo ""
echo "##### ALL DONE"
