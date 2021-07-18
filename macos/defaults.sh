#!/usr/bin/env bash

## backup before changing
defaults read >~/Documents/macos-defaults.txt

# Close any open System Preferences panes, to prevent them from overriding
# settings we’re about to change
osascript -e 'tell application "System Preferences" to quit'

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.macos` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

#########################
### * macOS defaults
#########################

# Disable the sound effects on boot
sudo nvram SystemAudioVolume=" "

# Save to disk by default, instead of iCloud
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Increase key repeat rate
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15


#########################
### * Finder
#########################

# Prefer Finder tabs: Dock -> Prefer tabs when opening documents
defaults write NSGlobalDomain AppleWindowTabbingMode -string "always"

# Finder: show path bar
defaults write com.apple.finder ShowPathbar -bool true

# Finder: show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Show the ~/Library folder
chflags nohidden ~/Library


########################
### * Menu bar, Dock, Dashboard, and hot corners
########################

# Auto-hide menu bar
defaults write NSGlobalDomain _HIHideMenuBar -bool true

# Don’t show recent applications in Dock
defaults write com.apple.dock show-recents -bool false

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

echo "Done. Note that some of these changes require a logout/restart to take effect."
