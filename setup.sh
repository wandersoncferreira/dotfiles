#!/usr/bin/env bash

directories=(
    zsh
    clojure
    gpg
    emacs
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

echo ""
echo "##### ALL DONE"
