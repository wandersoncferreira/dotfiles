#!/bin/env bash

fedoraFiles=$HOME/dotfiles/fedora

echo "Update Fedora"
sudo dnf update

echo "Install packages and Repositories"
sudo ${fedoraFiles}/packages.sh

echo "Start configuration"
${fedoraFiles}/config.sh
