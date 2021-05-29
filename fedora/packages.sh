#!/bin/env bash

# enable RPM Fusion repository
if [[ -z "$(dnf repolist | grep rpmfusion)" ]]
then
    dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
fi

# Command Line Applications
dnf -y install $(cat cli.txt)

# Fonts
dnf -y install $(cat fonts.txt)

# install nodejs and yarn
if [[ -z "$(dnf repolist | grep yarn)" ]]
   then
       curl --silent --location https://dl.yarnpkg.com/rpm/yarn.repo | sudo tee /etc/yum.repos.d/yarn.repo
       curl --silent --location https://rpm.nodesource.com/setup_12.x | sudo bash -
       dnf install yarn
fi

# enable flathub
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak -y install $(cat flatpak.txt)

# install docker
if [[ -z "$(rpm -qa | grep docker-ce)" ]]
then
    dnf -y install dnf-plugins-core
    dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
    dnf -y install docker-ce docker-ce-cli "containerd.io"
    systemctl start docker
    systemctl enable docker.service
    systemctl enable containerd.service
    groupadd docker
    usermod -aG docker $USER
    chmod 666 /var/run/docker.sock
fi

# docker compose
dnf -y install docker-compose
