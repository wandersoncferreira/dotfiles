#!/bin/env bash

dotfilesDir=$HOME/dotfiles

# change default shell to Zsh
chsh wanderson -s $(which zsh)
echo $(grep wanderson "/etc/passwd")

# get oh-my-zsh and autosuggestions
if [[ ! -d "$HOME/.oh-my-zsh" ]]
then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
fi

# setup zshrc
ln -sf $HOME/dotfiles/fedora/zshrc $HOME/.zshrc

# setup clj-kondo
if [[ ! -f "$HOME/bin/clj-kondo" ]]
   then
       curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
       chmod +x install-clj-kondo
       ./install-clj-kondo --dir /home/wanderson/bin
       rm install-clj-kondo
fi

# setup clojure
mkdir -p $HOME/.clojure
mkdir -p $HOME/.config/clj-kondo
mkdir -p $HOME/.lsp

ln -sf ${dotfilesDir}/clojure/deps.edn $HOME/.clojure/deps.edn
ln -sf ${dotfilesDir}/clojure/rebl-0.9.242.jar $HOME/.clojure/rebl-0.9.242.jar
ln -sf ${dotfilesDir}/clojure/clj-kondo/config.edn $HOME/.config/clj-kondo/config.edn
ln -sf ${dotfilesDir}/clojure/lsp/config.edn $HOME/.lsp/config.edn

# setup git
ln -sf ${dotfilesDir}/fedora/gitconfig $HOME/.gitconfig

## dconf
# repeat rate
dconf write /org/gnome/desktop/peripherals/keyboard/repeat true
dconf write /org/gnome/desktop/peripherals/keyboard/repeat-interval 30
dconf write /org/gnome/desktop/peripherals/keyboard/delay 400
