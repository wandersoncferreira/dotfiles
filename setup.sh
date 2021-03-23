#!/usr/bin/env bash


if [ -z "$1" ]; then
    echo "First arg must be 'reifyhealth'";
    exit 1
fi


directories=(
    zsh
    clojure
    gpg
    emacs
    git
)

stowit() {
    usr=$1
    app=$2
    # -v verbose
    # -R recursive
    # -t target
    stow -v -R -t ${usr} ${app}
}

echo ""
echo "Stowing apps for user: ${whoami}"

for app in ${directories[@]}; do
    stowit "${HOME}" $app
done

echo "setup nix"

sudo ln -sf $HOME/repos/personal/dotfiles/nix/configuration.nix /etc/nixos/configuration.nix
sudo ln -sf $HOME/repos/personal/dotfiles/nix/configurations /etc/nixos/configurations
sudo ln -sf $HOME/repos/personal/dotfiles/nix/home-managers/$1.nix $HOME/.config/nixpkgs/home.nix 

echo ""
echo "##### ALL DONE"
