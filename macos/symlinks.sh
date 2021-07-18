#!/usr/bin/env bash

cfg_path=$HOME/dotfiles/macos

ln -sf $cfg_path/zshrc $HOME/.zshrc
ln -sf $cfg_path/gnupg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf
ln -sf $cfg_path/gnupg/gpg.conf $HOME/.gnupg/gpg.conf
