#!/usr/bin/env bash

# all the symlinks will overwrite current existing ones

# emacs
ln -sfnv $PWD/emacs.d $HOME/.emacs.d

# kondo
ln -sfnv $PWD/clj-kondo $HOME/.config/clj-kondo

# gitconfig
ln -sfnv $PWD/.gitconfig  $HOME

# zshrc
ln -sfnv $PWD/zshrc $HOME/.zshrc

# gpg
ln -sfnv $PWD/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf

# if gpg-agent is not running you can start it with
# gpg-agent --daemon

# ledger
cd ..
ln -sfnv $PWD/ledger $HOME/ledger
cd configs

# files
gpg -d -o myFiles.tar.gz $PWD/files.tar.gz.gpg
tar -xvf $PWD/myFiles.tar.gz --one-top-level=niceties
ln -sfnv $PWD/niceties $HOME/.niceties
rm -f $PWD/myFiles.tar.gz
