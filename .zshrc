# Theo's Zsh config file.

# Prompt
autoload -U colors && colors
PS1="%B%{$fg[white]%}[%{$fg[yellow]%}%n%{$fg[white]%}@%{$fg[red]%}%M ${fg[green]%}%~%{$fg[white]%}]%{$reset_color%}$%b "

# Set history settings.
HISTFILE=~/.cache/zsh/hist
ZSH_COMPDUMP='~/.cache/zsh/zcompdump'
HISTSIZE=100000
SAVEHIST=100000

# Options
setopt autocd
unsetopt beep
bindkey -e
stty stop undef
setopt interactive_comments

zstyle :compinstall filename '/home/theodore/.zshrc'

autoload -Uz compinit
compinit

alias ls='ls --color=auto'
alias la='ls -las --color=auto'

alias sudo='sudo '

alias ttymacs='emacs --no-window-system'

source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
