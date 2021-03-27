# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="false"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=30

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git
         archlinux
         command-not-found
         common-aliases
         direnv
         docker-compose
         docker)

# page for common aliases
# https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/common-aliases

# aliases for docker-compose
# https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/docker-compose

# aliases for git plugin
# https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/git

export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacsclient -n'
else
  export EDITOR='emacsclient -n'
fi

# aliases
alias sz="source ~/.zshrc"
alias appsaucekey="ssh-add ~/.secrets/keys/app_saude_rsa"
alias flexianakey="ssh-add ~/.secrets/keys/flexiana_key"

export PATH="/home/wanderson/repos/personal/dotfiles/scripts:$PATH"

# enable gpg
export GPG_TTY=$(tty)

# enable direnv
eval "$(direnv hook zsh)"
