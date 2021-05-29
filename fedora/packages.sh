#!/bin/env bash

# enable RPM Fusion repository
dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

# Command Line Applications
dnf -y install $(cat cli.txt)

# Fonts
dnf -y install $(cat fonts.txt)

# install nodejs and yarn
curl --silent --location https://dl.yarnpkg.com/rpm/yarn.repo | sudo tee /etc/yum.repos.d/yarn.repo
curl --silent --location https://rpm.nodesource.com/setup_12.x | sudo bash -
dnf install yarn

# enable flathub
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

flatpak -y install $(cat flatpak.txt)
