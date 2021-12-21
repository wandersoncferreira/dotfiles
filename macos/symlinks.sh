#!/usr/bin/env bash

cfg_path=$HOME/code/dotfiles/macos

ln -sf $cfg_path/zshrc $HOME/.zshrc
ln -sf $cfg_path/gnupg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf
ln -sf $cfg_path/gnupg/gpg.conf $HOME/.gnupg/gpg.conf
ln -sf $cfg_path/ssh/config $HOME/.ssh/config
ln -sf $cfg_path/gitconfig $HOME/.gitconfig
